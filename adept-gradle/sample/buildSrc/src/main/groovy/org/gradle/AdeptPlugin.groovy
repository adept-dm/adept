package org.gradle

import org.gradle.api.Project
import org.gradle.api.Plugin

class AdeptPlugin implements Plugin<Project> {
    void apply(Project target) {
        target.task('buildTree', type: AdeptTask)
    }
}
