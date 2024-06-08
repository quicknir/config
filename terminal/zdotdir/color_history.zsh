highlight_to_format() {
    local parts=(${(s/,/)1})
    local before=""
    local after=""
    for part in $parts; do
        case "$part" in
            underline)
                before+="%U"
                after+="%u"
                ;;
            bold)
                before+="%B"
                after+="%b"
                ;;
            fg*)
                local sub_parts=(${(s/=/)part})
                before+="%F{$sub_parts[2]}"
                after+="%f"
                ;;
            bg*)
                local sub_parts=(${(s/=/)part})
                before+="%K{$sub_parts[2]}"
                after+="%k"
                ;;
        esac
    done
    pre_escape=${(%)before}
    post_escape=${(%)after}
}

apply_format_to_substr() {
    local mapped_first=$index_map[$first]
    local mapped_last=$index_map[$last]
    s=$1
    local insert_string=${pre_escape}${s[$mapped_first,$mapped_last]}${post_escape}
    s[$mapped_first,$mapped_last]=$insert_string
}

highlight_to_str() {
    local str=$1
    local highlight_arr_name=$2
    local -A index_map
    local str_length=${#str}
    local i pre_escape post_escape s v

    for i in {1..${#str}}; do index_map[$i]=$i; done

    for highlight in ${(P)${highlight_arr_name}}; do
        local parts=(${(s/ /)highlight})

        if [[ ${#parts} != 3 ]]; then 
            # Sometimes we get bad responses from fast-syntax highlighting
            # e.g. if fed '($index++)', we get
            # 0 1 fg=yellow 1 9 fg=red,bold 9 10 fg=yellow 0 1 fg=green,bold 9 10 fg=green,bold 9 10 bg=blue  1 bg=blue
            # That trailing style doesn't have a begin and end; so we just ignore it
            continue
        fi

        local first=$((parts[1]+1))
        local last=$parts[2]

        if [[ $first -ge $last ]]; then
            # Again, fast-syntax sometimes includes segments like this where start is greater than end
            # have observed it with git commit -m <quoted text>
            continue
        fi

        highlight_to_format $parts[3]

        local pre_escape_len=${#pre_escape}
        local post_escape_len=${#post_escape}
        apply_format_to_substr $str
        str=${(%)s}
        for i in {$first..$last}; do
            v=$index_map[$i]
            index_map[$i]=$((v+pre_escape_len))
        done
        if [[ $last != $str_length ]]; then
            for i in {$((last+1))..$str_length}; do
                v=$index_map[$i]
                index_map[$i]=$((v+pre_escape_len+post_escape_len))
            done
        fi
    done
    echo -n $str
}

# the setup of fast-syntax-highlight calls was taken from here
# https://github.com/zdharma-continuum/fast-syntax-highlighting/blob/master/test/parse.zsh#L180
--fast-syntax-highlight-str() {
    emulate -L zsh
    reply=()
    -fast-highlight-init

    local right_brace_is_recognized_everywhere
    integer path_dirs_was_set multi_func_def ointeractive_comments
    -fast-highlight-fill-option-variables

    -fast-highlight-process "" "$1" "0"
    -fast-highlight-string-process "" "$1"

    highlight_to_str $1 reply
}

make_hist_color_file() {
    emulate -L zsh
    # sometimes get some crazy git dump the first time we syntax highlight some git stuff
    -fast-highlight-process "" 'git diff foo' "0"

    rm -f "${HISTFILE}.color"

    local line
    for line in ${(Oa)history[@]}; do
        --fast-syntax-highlight-str $line >>! "${HISTFILE}.color"
        echo '' >>! "${HISTFILE}.color"
    done
}

--add-color-history-entry() {
    emulate -L zsh
    if [[ ${#1} > 1 ]]; then
        # sometimes get some crazy git dump the first time we syntax highlight some git stuff
        -fast-highlight-process "" 'git diff foo' "0"
        --fast-syntax-highlight-str $1 >>! "${HISTFILE}.color"
    fi
}

autoload -Uz add-zsh-hook
add-zsh-hook zshaddhistory --add-color-history-entry