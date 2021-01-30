#include <dlfcn.h>

#include <android/log.h>

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

#include "AlienWorksNative.h"

/* adapted from ecl.h */
typedef void *cl_object;

typedef int (*cl_boot_t)(int argc, char **argv);

typedef void (*cl_shutdown_t)(void);

typedef void (*alienworks_module_init_t)(cl_object);

typedef cl_object (*ecl_init_module_t)(cl_object block, void (*entry)(cl_object));

static cl_boot_t cl_boot;
static cl_shutdown_t cl_shutdown;
static ecl_init_module_t ecl_init_module;
static alienworks_module_init_t init_alienworks;

static int pfd[2];
static pthread_t thr;
static const char *tag = "EXCITING_DEMO";

static void *thread_func(void *data) {
    ssize_t rdsz;
    char buf[128];
    while ((rdsz = read(pfd[0], buf, sizeof buf - 1)) > 0) {
        if (buf[rdsz - 1] == '\n') --rdsz;
        buf[rdsz] = 0;  /* add null-terminator */
        __android_log_write(ANDROID_LOG_DEBUG, tag, buf);
    }
    return 0;
}

static int start_logger() {
    /* make stdout line-buffered and stderr unbuffered */
    setvbuf(stdout, 0, _IONBF, 0);
    setvbuf(stderr, 0, _IONBF, 0);

    /* create the pipe and redirect stdout and stderr */
    pipe(pfd);
    dup2(pfd[1], 1);
    dup2(pfd[1], 2);

    /* spawn the logging thread */
    if (pthread_create(&thr, 0, thread_func, 0) != 0) {
        return -1;
    }
    pthread_detach(thr);
    return 0;
}


static void *load_library(const char *library_name, int flags) {
    printf("Loading library %s\n", library_name);
    void *lib = dlopen(library_name, flags);
    if (!lib) {
        printf("Failed to load %s: %s\n", library_name, dlerror());
        assert(0);
    }
    return lib;
}

static void load_library_globally(const char *library_name) {
    load_library(library_name, RTLD_NOW | RTLD_GLOBAL);
}

static void *load_library_locally(const char *library_name) {
    return load_library(library_name, RTLD_NOW | RTLD_LOCAL);
}

static void load_ecl() {
    void *ecl_module = load_library_locally("libecl.so");
    cl_boot = dlsym(ecl_module, "cl_boot");
    cl_shutdown = dlsym(ecl_module, "cl_shutdown");
    ecl_init_module = dlsym(ecl_module, "ecl_init_module");
}

static void load_alienworks() {
    void *alien_works_module = load_library_locally("libalienworks.so");
    printf("WTF: %s\n", dlerror());
    /* must be exported by lisp side */
    init_alienworks = dlsym(alien_works_module, "__alien_works_android_init");
}

static void load_global_libraries(JNIEnv *env, jobjectArray libraries) {
    int library_count = (*env)->GetArrayLength(env, libraries);
    for (int i = 0; i < library_count; ++i) {
        jstring string = (jstring) ((*env)->GetObjectArrayElement(env, libraries, i));
        const char *library_name = (*env)->GetStringUTFChars(env, string, 0);
        load_library_globally(library_name);
    }
}

void Java_org_borodust_alienworks_android_AlienWorksNative_init(JNIEnv *env,
                                                                jclass clazz,
                                                                jobjectArray libraries) {
    start_logger();
    printf("API VERSION: %d\n", __ANDROID_API__);
    load_ecl();
    load_global_libraries(env, libraries);
    printf("GLM: %p\n", dlsym(NULL, "__claw__ZN3glm3vecILi3ExLNS_9qualifierE3EEppEi"));
    printf("SDL: %p\n", dlsym(NULL, "SDL_Init"));
    load_alienworks();
}

int __alien_works_android_main(int argc, char **argv) {
    cl_boot(argc, argv);

    printf("Almost there\n");
    ecl_init_module(NULL, init_alienworks);

    cl_shutdown();
    return 0;
}


