BASE="$(cd `dirname $0`; pwd)"
erl -args_file "$BASE/config/vm.args" -config "$BASE/config/sys.config"