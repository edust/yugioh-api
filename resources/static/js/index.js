/************************************
 * API 调用                          *
 ************************************/

function onKanaClick() {
    let name = $('#txtText').val();
    let type = $('input[name="rbType"]:checked').val();
    if (!name || name.trim() === '') return;
    let url = 'search';
    if (type === '1') {
        url = 'effect';
    } else if (type === '2') {
        url = 'normal';
    }
    $.ajax({
        url: `/kk/${url}`,
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify({name: name}),
        success: res => {
            let str = res.found ? res.kk : '没有找到注音'
            $('#txtKana').val(str);
        }
    });
}

function getCommonData() {
    $.ajax({
        url: '/api/common/count',
        type: 'GET',
        dataType: 'json',
        success: (res) => {
            $('#spCardCount')[0].innerHTML = res.cardCount;
            $('#spKanaCount')[0].innerHTML = res.kanaCount;
            $('#spSetCount')[0].innerHTML = res.setCount;
        }
    });
}