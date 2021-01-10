#include <dlfcn.h>

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

static void run(int argc, char **argv) {
    cl_boot(argc, argv);

    ecl_init_module(NULL, init_alienworks);

    cl_shutdown();
}

int __alien_works_android_main(int argc, char **argv) {
    void *ecl_module = dlopen("libecl.so", RTLD_NOW | RTLD_GLOBAL);
    void *alien_works_module = dlopen("libalienworks.so", RTLD_NOW | RTLD_GLOBAL);

    cl_boot = dlsym(ecl_module, "cl_boot");
    cl_shutdown = dlsym(ecl_module, "cl_shutdown");
    ecl_init_module = dlsym(ecl_module, "ecl_init_module");
    /* must be exported by lisp side */
    init_alienworks = dlsym(alien_works_module, "__alien_works_android_init");

    run(argc, argv);

    dlclose(alien_works_module);
    dlclose(ecl_module);
    return 0;
}


