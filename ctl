function start {
    local CMD="erl -pa ./ebin -s server start";
    bash -c "${CMD}";
    echo "ok"
}

case $1 in
    start)
        start;;
    *)
        echo "ok";;
esac
