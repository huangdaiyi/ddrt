BASE="$(cd `dirname $0`; pwd)"

CONFIG=" -noshell -sname ddrt -setcookie ddrt_dev +K true -kernel error_logger silent -pa $BASE/ebin -pa $BASE/deps/neg_hydra/ebin -s neg_hydra"

echo "erl "$CONFIG" -config "$BASE"/config/sys.config"
erl $CONFIG -config "$BASE/config/sys.config"
