
CONFIG=" -noshell -sname ddrt -setcookie ddrt_dev +K true -kernel error_logger silent -pa ebin -pa deps/neg_hydra/ebin -s neg_hydra"

echo "erl "$CONFIG" -config  config/sys.config"
erl $CONFIG -config "config/sys.config"
