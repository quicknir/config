#compdef bat
# NF - taken from one of the bat releases, 0.24.0

local context state state_descr line
typeset -A opt_args

(( $+functions[_bat_cache_subcommand] )) ||
_bat_cache_subcommand() {
    local -a args
    args=(
        '(-b --build -c --clear)'{-b,--build}'[Initialize or update the syntax/theme cache]'
        '(-b --build -c --clear)'{-c,--clear}'[Remove the cached syntax definitions and themes]'
        '(--source)'--source='[Use a different directory to load syntaxes and themes from]:directory:_files -/'
        '(--target)'--target='[Use a different directory to store the cached syntax and theme set]:directory:_files -/'
        '(--blank)'--blank'[Create completely new syntax and theme sets]'
        '(: -)'{-h,--help}'[Prints help information]'
        '*: :'
    )

    _arguments -S -s $args
}

(( $+functions[_bat_main] )) ||
_bat_main() {
    local -a args
    args=(
        '(-A --show-all)'{-A,--show-all}'[Show non-printable characters (space, tab, newline, ..)]'
        '*'{-p,--plain}'[Show plain style (alias for `--style=plain`), repeat twice to disable disable automatic paging (alias for `--paging=never`)]'
        '(-l --language)'{-l+,--language=}'[Set the language for syntax highlighting]:<language>:->language'
        '(-H --highlight-line)'{-H,--highlight-line}'[Highlight lines N through M]:<N\:M>...'
        '(--file-name)'--file-name'[Specify the name to display for a file]:<name>...:_files'
        '(-d --diff)'--diff'[Only show lines that have been added/removed/modified]'
        '(--diff-context)'--diff-context'[Include N lines of context around added/removed/modified lines when using `--diff`]:<N> (lines):()'
        '(--tabs)'--tabs'[Set the tab width to T spaces]:<T> (tab width):()'
        '(--wrap)'--wrap='[Specify the text-wrapping mode]:<when>:(auto never character)'
        '(--terminal-width)'--terminal-width'[Explicitly set the width of the terminal instead of determining it automatically]:<width>'
        '(-n --number)'{-n,--number}'[Show line numbers]'
        '(--color)'--color='[When to use colors]:<when>:(auto never always)'
        '(--italic-text)'--italic-text='[Use italics in output]:<when>:(always never)'
        '(--decorations)'--decorations='[When to show the decorations]:<when>:(auto never always)'
        '(--paging)'--paging='[Specify when to use the pager]:<when>:(auto never always)'
        '(-m --map-syntax)'{-m+,--map-syntax=}'[Use the specified syntax for files matching the glob pattern]:<glob\:syntax>...'
        '(--theme)'--theme='[Set the color theme for syntax highlighting]:<theme>:->theme'
        '(: --list-themes --list-languages -L)'--list-themes'[Display all supported highlighting themes]'
        '(--style)'--style='[Comma-separated list of style elements to display]:<components>:->style'
        '(-r --line-range)'{-r+,--line-range=}'[Only print the lines from N to M]:<N\:M>...'
        '(: --list-themes --list-languages -L)'{-L,--list-languages}'[Display all supported languages]'
        '(: --no-config)'--no-config'[Do not use the configuration file]'
        '(: --no-custom-assets)'--no-custom-assets'[Do not load custom assets]'
        '(: --lessopen)'--lessopen'[Enable the $LESSOPEN preprocessor]'
        '(: --no-lessopen)'--no-lessopen'[Disable the $LESSOPEN preprocessor if enabled (overrides --lessopen)]'
        '(: --config-dir)'--config-dir'[Show bat'"'"'s configuration directory]'
        '(: --config-file)'--config-file'[Show path to the configuration file]'
        '(: --generate-config-file)'--generate-config-file'[Generates a default configuration file]'
        '(: --cache-dir)'--cache-dir'[Show bat'"'"'s cache directory]'
        '(: -)'{-h,--help}'[Print this help message]'
        '(: -)'{-V,--version}'[Show version information]'
        '*: :_files'
    )

    _arguments -S -s $args

    case "$state" in
        language)
            local IFS=$'\n'
            local -a languages
            languages=( $(bat --list-languages | awk -F':|,' '{ for (i = 1; i <= NF; ++i) printf("%s:%s\n", $i, $1) }') )

            _describe 'language' languages
        ;;

        theme)
            local IFS=$'\n'
            local -a themes
            themes=( $(bat --list-themes | sort) )

            _values 'theme' $themes
        ;;

        style)
            _values -s , 'style' default auto full plain changes header header-filename header-filesize grid rule numbers snip
        ;;
    esac
}

case $words[2] in
    cache)
        ## Completion of the 'cache' command itself is removed for better UX
        ## See https://github.com/sharkdp/bat/issues/2085#issuecomment-1271646802
        _bat_cache_subcommand
    ;;

    *)
        _bat_main
    ;;
esac
