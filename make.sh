#!/bin/sh

fpc -MObjFPC -Scghi -O1 -gw -gl -l -vewnhibq \
        -Filib/x86_64-linux \
        -Fiiscbase \
        -Fiiscbase/database \
        -Fiiscbase/database/component \
        -Fiiscbase/database/core \
        -Fiiscbase/database/dbc \
        -Fiiscbase/database/parsesql \
        -Fiiscbase/database/plain \
        -Fiiscbase/network \
        -Fiiscbase/script \
        -Fiiscbase/lazutils \
        -Fusrc \
        -Fuiscbase \
        -Fuiscbase/database \
        -Fuiscbase/database/component \
        -Fuiscbase/database/core \
        -Fuiscbase/database/dbc \
        -Fuiscbase/database/parsesql \
        -Fuiscbase/database/plain \
        -Fuiscbase/network \
        -Fuiscbase/script \
        -Fuiscbase/coroutine \
        -Fuiscbase/websocket \
        -Fuiscbase/lazutils \
        -Fu. \
        -FUlib/x86_64-linux \
        -FE. \
        -oygosvr ygosvr.lpr

strip ygosvr


