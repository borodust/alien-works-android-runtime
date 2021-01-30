package org.borodust.alienworks.android

import org.borodust.alienworks.android.AlienWorksNative.init
import org.libsdl.app.SDL
import org.libsdl.app.SDLActivity

class AlienWorksActivity : SDLActivity() {

    override fun loadLibraries() {
        SDL.loadLibrary("main")
        init(arrayOf("libhidapi.so",
                     "libSDL2.so",
                     "libfilament.clawed.so",
                     "libglm.clawed.so"
        ))
        super.loadLibraries()
    }

    override fun getMainSharedObject(): String {
        return "libmain.so"
    }

    override fun getLibraries(): Array<String> {
        // libraries required to have symbols imported into JVM
        return arrayOf("hidapi",
                       "SDL2",
                       "main")
    }

    override fun getMainFunction(): String {
        return "__alien_works_android_main"
    }
}