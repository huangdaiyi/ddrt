
CONFIG=" -noshell -sname ddrt -setcookie ddrt_dev +K true -kernel error_logger silent -pa /ddrt/ebin -pa /ddrt/deps/neg_hydra/ebin -s neg_hydra"

echo "erl "$CONFIG" -config  /ddrt/config/sys.config"
erl $CONFIG -config "/ddrt/config/sys.config"
