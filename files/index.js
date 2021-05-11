/************************************
 * API 调用                          *
 ************************************/

function onKanaClick() {
    let name = $('#txtText').val();
    let type = $('input[name="rbType"]:checked').val();
    if (!name || name.trim() === '') return;
    let url = 'name';
    if (type === '1') {
        url = 'effect';
    } else if (type === '2') {
        url = 'text';
    }
    $.ajax({
        url: `/api/kanjikana/${url}`,
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify({name: name}),
        dataType: 'json',
        success: res => {
            let str = res.data;
            if (!str) {
                str = 'not found';
            }
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
            $('#spCardCount')[0].innerHTML = res.data.cardCount;
            $('#spKanaCount')[0].innerHTML = res.data.kanaCount;
            $('#spSetCount')[0].innerHTML = res.data.setCount;
        }
    });
}