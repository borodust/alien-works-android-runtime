package org.borodust.alienworks.android

import org.libsdl.app.SDLActivity

class AlienWorksActivity : SDLActivity() {
    override fun getLibraries(): Array<String> {
        return arrayOf("ecl",
                       "hidapi",
                       "SDL2",
                       "glm.clawed",
                       "filament.clawed",
                       "alienworks",
                       "main");
    }

    override fun getMainFunction(): String {
        return "__alien_works_android_main";
    }
}