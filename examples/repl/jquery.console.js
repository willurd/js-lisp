// JQuery Console 1.0
// Sun Feb 21 20:28:47 GMT 2010
//
// Copyright 2010 Chris Done. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
//    1. Redistributions of source code must retain the above
//       copyright notice, this list of conditions and the following
//       disclaimer.

//    2. Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials
//       provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY CHRIS DONE ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL CHRIS DONE OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
// OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
// BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
// USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
// DAMAGE.

// The views and conclusions contained in the software and
// documentation are those of the authors and should not be
// interpreted as representing official policies, either expressed or
// implied, of Chris Done.
//
// TESTED ON
//   Internet Explorer 6
//   Opera 10.01
//   Chromium 4.0.237.0 (Ubuntu build 31094)
//   Firefox 3.5.8

(function($){
    $.fn.console = function(config){
		var log = function () {
			var a = [];
			for (var i = 0; i < arguments.length; i++) {
				a.push(arguments[i]);
			}
			extern.message(a.join(" "), "jquery-console-stdout");
		};
		
	    // External exports object
		config = config || {};
	    var extern = {};
		
        ////////////////////////////////////////////////////////////////////////
        // Constants
        // Some are enums, data types, others just for optimisation
        extern.keyCodes = { left:37,right:39,up:38,down:40,back:8,del:46,
                            end:35,start:36,ret:13 };
        extern.cursor = '<span class="jquery-console-cursor">&nbsp;</span>';
        // Opera only works with this character, not <wbr> or &shy;,
        // but IE6 displays this character, which is bad, so just use
        // it on Opera.
        var wbr = $.browser.opera? '&#8203;' : '';

        ////////////////////////////////////////////////////////////////////////
        // Globals
        extern.container = $(this);
        var inner = $('<div class="jquery-console-inner"></div>');
        extern.typer = $('<input class="jquery-console-typer" type="text">');
        // Prompt
        extern.promptBox;
        extern.prompt;
        extern.ps1 = config.ps1 || ">> "; // Input
        extern.ps2 = config.ps2 || ".. "; // Multiline commands
		extern.ps3 = config.ps3 || "=> "; // Return values
		extern.promptLabel = extern.ps1; // The current label
        extern.column = 0;
        extern.promptText = '';
        extern.restoreText = '';
        // Prompt history stack
        extern.history = [];
        extern.ringn = 0;
        // For reasons unknown to The Sword of Michael himself, Opera
        // triggers and sends a key character when you hit various
        // keys like PgUp, End, etc. So there is no way of knowing
        // when a user has typed '#' or End. My solution is in the
        // typer.KEYDOWN and typer.keypress functions; I use the
        // variable below to ignore the keypress event if the keydown
        // event succeeds.
        var cancelKeyPress = 0;
		
		extern.multiLineCommand = false;
		extern.currentText = '';
		
        ////////////////////////////////////////////////////////////////////////
        // Reset terminal
        extern.reset = function (doFade) {
            var welcome = true;
			doFade = doFade || false;
			if (doFade) {
				inner.parent().fadeOut(function(){
	                inner.find('div').each(function(){
	                    if (!welcome) 
	                        $(this).remove();
	                    welcome = false;
	                });
	                extern.newPromptBox();
	                inner.parent().fadeIn(function(){
	                    inner.addClass('jquery-console-focus');
	                    extern.typer.focus();
	                });
	            });
			} else {
				inner.find('div').each(function(){
                    if (!welcome) 
                        $(this).remove();
                    welcome = false;
                });
                extern.newPromptBox();
			}
        };

        ////////////////////////////////////////////////////////////////////////
        // Reset terminal
        extern.notice = function (msg, style, animate){
            var n = $('<div class="notice"></div>').append($('<div></div>').text(msg))
                .css({visibility:'hidden'});
            extern.container.append(n);
            var focused = true;
            if (style=='fadeout')
                setTimeout(function(){
                    n.fadeOut(function(){
                        n.remove();
                    });
                },4000);
            else if (style=='prompt') { 
                var a = $('<br/><div class="action"><a href="javascript:">OK</a><div class="clear"></div></div>');
                n.append(a);
                focused = false;
                a.click(function(){ n.fadeOut(function(){ n.remove();inner.css({opacity:1}) }); });
            }
            var h = n.height();
			if (animate) {
    	        n.css({height:'0px',visibility:'visible'})
	                .animate({height:h+'px'},function(){
	                    if (!focused) inner.css({opacity:0.5});
	                });
			} else {
	            n.css({height:h+'px',visibility:'visible'});
			}
            n.css('cursor','default');
            return n;
        };

        ////////////////////////////////////////////////////////////////////////
        // Make a new prompt box
        extern.newPromptBox = function () {
            extern.column = 0;
            extern.promptText = '';
            extern.promptBox = $('<div class="jquery-console-prompt-box"></div>');
            var label = $('<span class="jquery-console-prompt-label"></span>');
            extern.promptBox.append(label.text(extern.promptLabel).show());
            extern.prompt = $('<span class="jquery-console-prompt"></span>');
            extern.promptBox.append(extern.prompt);
            inner.append(extern.promptBox);
            updatePromptDisplay();
        };

        ////////////////////////////////////////////////////////////////////////
        // Handle setting focus
        extern.container.click(function(){
        	extern.typer.focus();
            inner.addClass('jquery-console-focus');
            inner.removeClass('jquery-console-nofocus');
            scrollToBottom();
            return false;
        });
		
        // Scroll to the bottom of the view
		// FIXME: This is borked on Chrome.
        function scrollToBottom () {
			//target.offsetParent().scrollTop(target.offset().top - 50);
			inner.attr({scrollTop: inner.attr("scrollHeight")});
        }
		
        ////////////////////////////////////////////////////////////////////////
        // Handle losing focus
        extern.typer.blur(function(){
            inner.removeClass('jquery-console-focus');
            inner.addClass('jquery-console-nofocus');
        });

        ////////////////////////////////////////////////////////////////////////
        // Handle key hit before translation
        // For picking up control characters like up/left/down/right

        extern.typer.keydown(function(e){
            cancelKeyPress = 0;
			extern.consoleControl(e);
        });
        
        ////////////////////////////////////////////////////////////////////////
        // Handle key press
        extern.typer.keypress(function(e){
            var keyCode = e.keyCode || e.which;
            if (cancelKeyPress != keyCode && keyCode >= 32){
                if (cancelKeyPress) return false;
                if (typeof config.charInsertTrigger == 'undefined' ||
                    (typeof config.charInsertTrigger == 'function' &&
                     config.charInsertTrigger(keyCode,extern.promptText)))
                    extern.typer.consoleInsert(keyCode);
            }
            if ($.browser.webkit) return false;
        });

        // Is a keycode a contorl character? 
        // E.g. up, down, left, right, backspc, return, etc.
        function isControlCharacter (keyCode) {
            // TODO: Make more precise/fast.
            return (
                (keyCode >= extern.keyCodes.left && keyCode <= extern.keyCodes.down)
                    || keyCode == extern.keyCodes.back || keyCode == extern.keyCodes.del
                    || keyCode == extern.keyCodes.end || keyCode == extern.keyCodes.start
                    || keyCode == extern.keyCodes.ret
            );
        };
		
		extern.consoleControl = function (e) {
			return extern.defaultConsoleControl(e);
		};
		
        ////////////////////////////////////////////////////////////////////////
        // Handle console control keys
        // E.g. up, down, left, right, backspc, return, etc.
		
		extern.cancelKey = function (keyCode) {
			cancelKeyPress = keyCode;
		};
		
        extern.defaultConsoleControl = function (e) {
            switch (e.keyCode) {
            case extern.keyCodes.left:{
				extern.cancelKey(e.keyCode);
                moveColumn(-1);
                updatePromptDisplay(); 
                return false;
                break;
            }
            case extern.keyCodes.right:{
				extern.cancelKey(e.keyCode);
                moveColumn(1); 
                updatePromptDisplay();
                return false;
                break; 
            }
            case extern.keyCodes.back:{
				extern.cancelKey(e.keyCode);
                if (moveColumn(-1)){
                    deleteCharAtPos();
                    updatePromptDisplay();
                }
                return false;
                break;
            }
            case extern.keyCodes.del:{
				extern.cancelKey(e.keyCode);
                if (deleteCharAtPos())
                    updatePromptDisplay();
                return false;
                break;
            }
            case extern.keyCodes.end:{
				extern.cancelKey(e.keyCode);
				extern.moveToEnd();
                return false;
                break;
            }
            case extern.keyCodes.start:{
				extern.cancelKey(e.keyCode);
				extern.moveToStart();
                return false;
                break;
            }
            case extern.keyCodes.ret:{
				extern.cancelKey(e.keyCode);
                commandTrigger(); return false;
            }
            case extern.keyCodes.up:{
				extern.cancelKey(e.keyCode);
                rotateHistory(-1); return false;
            }
            case extern.keyCodes.down:{
				extern.cancelKey(e.keyCode);
                rotateHistory(1); return false;
            }
            default: //alert("Unknown control character: " + keyCode);
            }
        };
		
		extern.moveToStart = function () {
            if (moveColumn(-extern.column))
                updatePromptDisplay();
		};
		
		extern.moveToEnd = function () {
            if (moveColumn(extern.promptText.length-extern.column))
                updatePromptDisplay();
		};
		
		extern.moveWordLeft = function () {
			var c = extern.column;
			var s = extern.promptText;
			var match = s.slice(0, c).match(/(^|\w+)\W*$/);
			if (match && match.length >= 1) {
				c -= match[0].length;
				extern.column = c;
				updatePromptDisplay();
			}
		};
		
		extern.moveWordRight = function () {
			var c = extern.column;
			var s = extern.promptText;
			var match = s.slice(c).match(/^\W*(\w+|$)/);
			if (match && match.length >= 1) {
				c += match[0].length;
				extern.column = c;
				updatePromptDisplay();
			}
		};
		
		extern.deleteWordLeft = function () {
			var c = extern.column;
			var s = extern.promptText;
			extern.moveWordLeft();
			extern.promptText = s.slice(0, extern.column) + s.slice(c);
			updatePromptDisplay();
		};
		
		extern.deleteWordRight = function () {
			var c = extern.column;
			var s = extern.promptText;
			extern.moveWordRight();
			extern.promptText = s.slice(0, c) + s.slice(extern.column);
			extern.column = c;
			updatePromptDisplay();
		};
		
        ////////////////////////////////////////////////////////////////////////
        // Rotate through the command history
        function rotateHistory(n){
            if (extern.history.length == 0) return;
            extern.ringn += n;
            if (extern.ringn < 0) extern.ringn = extern.history.length;
            else if (extern.ringn > extern.history.length) extern.ringn = 0;
            var prevText = extern.promptText;
            if (extern.ringn == 0) {
                extern.promptText = extern.restoreText;
            } else {
                extern.promptText = extern.history[extern.ringn - 1];
            }
            if (config.historyPreserveColumn) {
                if (extern.promptText.length < extern.column + 1) {
                    extern.column = extern.promptText.length;
                } else if (extern.column == 0) {
                    extern.column = extern.promptText.length;
                }
            } else if (config.historyColumnAtEnd) {
                extern.column = extern.promptText.length;
            } else {
                extern.column = 0;
            }
            updatePromptDisplay();
        };

        // Add something to the history ring
        function addToHistory(line){
            extern.history.push(line);
            extern.restoreText = '';
        };

        // Delete the character at the current position
        function deleteCharAtPos(){
            if (extern.promptText != ''){
                extern.promptText =
                    extern.promptText.substring(0,extern.column) +
                    extern.promptText.substring(extern.column+1);
                extern.restoreText = extern.promptText;
                return true;
            } else return false;
        };

        ////////////////////////////////////////////////////////////////////////
        // Validate command and trigger it if valid, or show a validation error
        function commandTrigger() {
            var line = extern.promptText;
            if (typeof config.commandValidate == 'function') {
                var ret = config.commandValidate(line);
                if (ret == true || ret == false) {
                    if (ret) {
                        handleCommand();
                    }
                } else {
                    extern.commandResult(ret,"jquery-console-message-error");
                }
            } else {
                handleCommand();
            }
        };
		
        ////////////////////////////////////////////////////////////////////////
        // Handle a command
        function handleCommand () {
            if (typeof config.commandHandle == 'function') {
				var text = extern.multiLineCommand ? extern.currentText + extern.promptText :
													 extern.promptText;
                var ret = config.commandHandle(text, function(msgs) {
                    extern.commandResult(msgs);
                });
				if (ret === null) { // This command needs more text
					extern.promptLabel = extern.ps2;
					extern.multiLineCommand = true;
					extern.currentText = text;
		            extern.column = -1;
		            updatePromptDisplay();
		            extern.newPromptBox();
				} else {
					extern.promptLabel = extern.ps1;
					
					if (typeof ret == 'boolean') {
	                    if (ret) {
	                        // Command succeeded without a result.
	                        addToHistory(text);
	                        extern.commandResult();
	                    } else {
	                        addToHistory(text);
	                        extern.commandResult('Command failed.', "jquery-console-message-error");
	                    }
	                } else if (typeof ret == "string") {
	                    addToHistory(text);
	                    extern.commandResult(ret, "jquery-console-message-success");
	                } else if (typeof ret == 'undefined') {
	                    addToHistory(text);
	                } else if (ret.length) {
	                    addToHistory(text);
	                    extern.commandResult(ret);
	                }
				}
			}
        }

        ////////////////////////////////////////////////////////////////////////
        // Reset the prompt in invalid command
        extern.commandResult = function (msg,className) {
			extern.multiLineCommand = false;
			extern.promptLabel = extern.ps1;
			extern.currentText = "";
            extern.column = -1;
            updatePromptDisplay();
            if (typeof msg == 'string') {
                extern.message(msg,className);
            } else {
                for (var x in msg) {
                    var ret = msg[x];
                    extern.message(extern.ps3 + ret.msg, ret.className);
                }
            }
            extern.newPromptBox();
        };

        ////////////////////////////////////////////////////////////////////////
        // Display a message
        extern.message = function (msg,className) {
            var mesg = $('<div class="jquery-console-message"></div>');
            if (className) mesg.addClass(className);
            mesg.filledText(msg).hide();
            inner.append(mesg);
            mesg.show();
        };

        ////////////////////////////////////////////////////////////////////////
        // Handle normal character insertion
        extern.typer.consoleInsert = function(keyCode){
            // TODO: remove redundant indirection
            var char = String.fromCharCode(keyCode);
            var before = extern.promptText.substring(0,extern.column);
            var after = extern.promptText.substring(extern.column);
            extern.promptText = before + char + after;
            moveColumn(1);
            extern.restoreText = extern.promptText;
            updatePromptDisplay();
        };
        
        ////////////////////////////////////////////////////////////////////////
        // Move to another column relative to this one
        // Negative means go back, positive means go forward.
        function moveColumn(n){
            if (extern.column + n >= 0 && extern.column + n <= extern.promptText.length){
                extern.column += n;
                return true;
            } else return false;
        };

        extern.setPromptText = function(text){
            if (text) {
                extern.promptText = text;
                if (extern.column > extern.promptText.length)
                    extern.column = extern.promptText.length;
                updatePromptDisplay();
            }
            return extern.promptText;
        };

        ////////////////////////////////////////////////////////////////////////
        // Update the prompt display
        function updatePromptDisplay(){
            var line = extern.promptText;
            var html = '';
            if (extern.column > 0 && line == ''){
                // When we have an empty line just display a cursor.
                html = extern.cursor;
            } else if (extern.column == extern.promptText.length){
                // We're at the end of the line, so we need to display
                // the text *and* cursor.
                html = htmlEncode(line) + extern.cursor;
            } else {
                // Grab the current character, if there is one, and
                // make it the current cursor.
                var before = line.substring(0, extern.column);
                var current = line.substring(extern.column,extern.column+1);
                if (current){
                    current = 
                        '<span class="jquery-console-cursor">' +
                        htmlEncode(current) +
                        '</span>';
                }
                var after = line.substring(extern.column+1);
                html = htmlEncode(before) + current + htmlEncode(after);
            }
            extern.prompt.html(html);
            scrollToBottom();
        };
        
        // Simple HTML encoding
        // Simply replace '<', '>' and '&'
        // TODO: Use jQuery's .html() trick, or grab a proper, fast
        // HTML encoder.
        function htmlEncode(text){
            return (
                text.replace(/&/g,'&amp;')
                    .replace(/</g,'&lt;')
                    .replace(/</g,'&lt;')
                    .replace(/ /g,'&nbsp;')
                    .replace(/([^<>&]{10})/g,'$1<wbr>&shy;' + wbr)
            );
        };
		
        ////////////////////////////////////////////////////////////////////////
        // Main entry point
        (function(){
            extern.container.append(inner);
            inner.append(extern.typer);
            extern.typer.css({position:'absolute',top:0,left:'-9999px'});
            if (config.welcomeMessage)
                extern.message(config.welcomeMessage,'jquery-console-welcome');
            extern.newPromptBox();
            if (config.autofocus) {
                inner.addClass('jquery-console-focus');
                extern.typer.focus();
                setTimeout(function(){
                    inner.addClass('jquery-console-focus');
                    extern.typer.focus();
                },100);
            }
            extern.inner = inner;
            extern.scrollToBottom = scrollToBottom;
        })();
		
        return extern;
    };
    // Simple utility for printing messages
    $.fn.filledText = function(txt){
        $(this).text(txt);
        $(this).html($(this).html().replace(/\n/g,'<br/>'));
        return this;
    };
})(jQuery);
