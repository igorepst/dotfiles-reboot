_mvn_projects() {
    local pom_file=$(__mvn_get_parent_pom_file) ret=1
    local build_order=.mvnBuildOrder.tmp
    if [[ -f $pom_file ]]; then
        if [[ -f ${build_order} ]]; then
            zmodload zsh/mapfile
            local projects_arr=("${(f)mapfile[${build_order}]}")
            local projects=(${(z)projects_arr[1]})
            compset -P ':'
        else 
            setopt localoptions extendedglob
            local projects; projects=(${pom_file:h}/*/**/pom.xml~*target\/*) # FIXME project.build.directory is not always target/
            projects=(${${projects#.\/}:h})
        fi
        [[ $#projects -gt 0 ]] && _values "$@" 'project' "${projects[@]}" && ret=0
    fi
    return ret
}
