/************************************
 * API 调用                          *
 ************************************/

function kana(name /* string */, callback /* (string) -> void */) {
    if (name.trim() === '') {
        callback('');
        return;
    }
    $.ajax({
        url: `/api/yugioh/kana?name=${encodeURI(name)}`,
        type: 'GET',
        dataType: 'json',
        success: (res) => {
            callback(res.kana);
        }
    });
}

function addKana(name /* string */, kana /* string */, callback /* (bool) -> void */) {
    if (name.trim() === '' || kana.trim() === '') {
        callback(false);
        return;
    }
    $.ajax({
        url: '/api/yugioh/kana/add',
        type: 'POST',
        dataType: 'json',
        contentType: 'application/json',
        data: JSON.stringify({
            name: name,
            kana: kana
        }) ,
        success: (res) => {
            let succ = res.message === 'OK';
            callback(succ);
        }
    });
}

function deleteKana(id /* int */, callback /* (bool) -> void */) {
    if (id <= 0) {
        callback(false);
        return;
    }
    $.ajax({
        url: '/api/yugioh/kana/delete',
        type: 'POST',
        dataType: 'json',
        contentType: 'application/json',
        data: JSON.stringify({
            id: id
        }) ,
        success: (res) => {
            let succ = res.message === 'OK';
            callback(succ);
        }
    });
}

function listKana(callback /* (Array<{id: int, name: string, kana: string}>) -> void */) {
    $.ajax({
        url: '/api/yugioh/kana/list',
        type: 'GET',
        dataType: 'json',
        success: (res) => {
            callback(res.list);
        }
    });
}

function searchKana(key /* string */, callback /* (Array<{id: int, name: string, kana: string}>) -> void */) {
    if (key.trim() === '') {
        callback([]);
        return;
    }
    console.log(key);
    $.ajax({
        url: `/api/yugioh/kana/search?key=${encodeURI(key)}`,
        type: 'GET',
        dataType: 'json',
        success: (res) => {
            callback(res.list);
        }
    });
}

function reloadData(callback /* (bool) -> void */) {
    $.ajax({
        url: '/api/yugioh/kana/reload',
        type: 'GET',
        dataType: 'json',
        success: (res) => {
            let succ = res.message === 'OK';
            callback(succ);
        }
    });
}

/************************************
 * 按钮事件                          *
 ************************************/

function onKanaClick() {
    let name = $('#txtName').val();
    kana(name, (kana) => {
        $('#txtKana').val(kana);
    });
}

function onAddKanaClick() {
    let name = $('#txtAddName').val();
    let kana = $('#txtAddKana').val();
    console.log(name);
    console.log(kana);
    addKana(name, kana, (succ) => {

        alert(succ ? 'ADDED' : 'ERROR');
    });
}

function onDeleteClick(id /* int */) {
    deleteKana(id, (succ) => {
        alert(succ ? 'DELETED' : 'ERROR');
    });
}

function onListKanaClick() {
    listKana((list) => buildKanaList(list));
}

function onSearchKanaClick() {
    let key = $('#txtSearch').val();
    searchKana(key, (list) => buildKanaList(list));
}

function buildKanaList(list /* Array<{id: int, name: string, kana: string}> */) {
    console.log(list);
    let str = '';
    list.forEach((item) => {
        str += `<tr><td>${item.id}</td><td>${item.name}</td><td>${item.kana}</td><td><button onclick="onDeleteClick(${item.id});">DEL</button></td></tr>`;
    });
    $('#tblListKana')[0].innerHTML = str;
}

function onReloadClick() {
    reloadData((succ) => {
        alert(succ ? 'RELOADED' : 'ERROR');
    });
}