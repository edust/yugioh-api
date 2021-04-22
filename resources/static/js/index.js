/************************************
 * API 调用                          *
 ************************************/

function onKanaClick() {
    let name = $('#txtName').val();
    if (!name || name.trim() === '') return;
    $.ajax({
        url: '/kk/search',
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify({name: name}),
        success: res => {
            let str = res.found ? res.kk : '没有找到卡名'
            $('#txtKana').val(str);
        }
    });
}

function onEffectKanaClick() {
    let effect = $('#txtEffect').val();
    if (!effect || effect.trim() === '') return;
    $.ajax({
        url: '/kk/effect',
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify({name: effect}),
        success: res => {
            let str = res.found ? res.kk : '没有找到卡名'
            $('#txtEffectKana').val(str);
        }
    });
}