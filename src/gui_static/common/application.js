// ===================================================================
// Author: Lukasz Opiola
// Copyright (C): 2014 ACK CYFRONET AGH
// This software is released under the MIT license
// cited in 'LICENSE.txt'.
// ===================================================================
// This file contains JS functions commonly used on multiple pages.
// ===================================================================

// Applies a function after a timeout, but the timer is
// reset every time the function is called within it's countdown.
function debounce(func, wait) {
    var timeout;
    return function() {
        var context = this,
            args = arguments,
            later = function() {
                timeout = null;
                func.apply(context, args);
            };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}

// -----------------------------
// top menu scroll handling

initialize_top_menu = function () {
    scroll_top_menu();
    $(window).resize(function () {
        if ($(window).width() >= $(document).width()) {
            $('#top_menu').css('left', '0px').css('right', '0px');
        } else {
            scroll_top_menu();
        }
    });
    $(window).scroll(function () {
        scroll_top_menu();
    });
};

scroll_top_menu = function () {
    if ($(window).width() < $(document).width()) {
        $('#top_menu').css('left', (-1 * $(this).scrollLeft()) + 'px')
            .css('right', ($(this).scrollLeft() - $(document).width() + $(window).width()) + 'px');
    }
};