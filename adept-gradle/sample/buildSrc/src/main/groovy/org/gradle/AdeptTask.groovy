package org.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction

class AdeptTask extends DefaultTask {

    @TaskAction
    def buildTree() {
        println dependencies
    }
}