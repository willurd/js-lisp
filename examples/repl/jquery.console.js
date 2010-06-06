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
    $.fn.console = function (config) {
		var log = function () {
			var a = [];
			for (var i = 0; i < arguments.length; i++) {
				a.push(arguments[i]);
			}
			self.message(a.join(" "), "jquery-console-stdout");
		};
		
	    // External exports object
		config = config || {};
	    var self = {};
		
        ////////////////////////////////////////////////////////////////////////
        // Constants
        // Some are enums, data types, others just for optimisation
        self.keyCodes = { left:37,right:39,up:38,down:40,back:8,del:46,
                            end:35,start:36,ret:13 };
        self.cursor = '<span class="jquery-console-cursor">&nbsp;</span>';
        // Opera only works with this character, not <wbr> or &shy;,
        // but IE6 displays this character, which is bad, so just use
        // it on Opera.
        var wbr = $.browser.opera? '&#8203;' : '';

        ////////////////////////////////////////////////////////////////////////
        // Globals
        self.container = $(this);
        var inner = $('<div class="jquery-console-inner"></div>');
        self.typer = $('<input class="jquery-console-typer" type="text">');
        // Prompt
        self.promptBox;
        self.prompt;
        self.ps1 = config.ps1 || ">> "; // Input
        self.ps2 = config.ps2 || ".. "; // Multiline commands
		self.ps3 = config.ps3 || "=> "; // Return values
        self.column = 0;
        self.promptText = '';
        self.restoreText = '';
        // Prompt history stack
        self.history = [];
        self.historyCursor = 0;
        // For reasons unknown to The Sword of Michael himself, Opera
        // triggers and sends a key character when you hit various
        // keys like PgUp, End, etc. So there is no way of knowing
        // when a user has typed '#' or End. My solution is in the
        // typer.KEYDOWN and typer.keypress functions; I use the
        // variable below to ignore the keypress event if the keydown
        // event succeeds.
        var cancelKeyPress = 0;
		
		self.multiLineCommand = false;
		self.currentText = '';
		
        ////////////////////////////////////////////////////////////////////////
        // Reset terminal
        self.reset = function (doFade) {
            var welcome = true;
			doFade = doFade || false;
			if (doFade) {
				inner.parent().fadeOut(function(){
	                inner.find('div').each(function(){
	                    if (!welcome) 
	                        $(this).remove();
	                    welcome = false;
	                });
	                self.newPromptBox();
	                inner.parent().fadeIn(function(){
	                    inner.addClass('jquery-console-focus');
	                    self.typer.focus();
	                });
	            });
			} else {
				inner.find('div').each(function(){
                    if (!welcome) 
                        $(this).remove();
                    welcome = false;
                });
                self.newPromptBox();
			}
        };

        ////////////////////////////////////////////////////////////////////////
        // Reset terminal
        self.notice = function (msg, style, animate){
            var n = $('<div class="notice"></div>').append($('<div></div>').text(msg))
                .css({visibility:'hidden'});
            self.container.append(n);
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
        self.newPromptBox = function () {
			var promptLabel = self.multiLineCommand ? self.ps2 : self.ps1;
            self.column = 0;
            self.promptText = '';
            self.promptBox = $('<div class="jquery-console-prompt-box"></div>');
            var label = $('<span class="jquery-console-prompt-label"></span>');
            self.promptBox.append(label.text(promptLabel).show());
            self.prompt = $('<span class="jquery-console-prompt"></span>');
            self.promptBox.append(self.prompt);
            inner.append(self.promptBox);
            updatePromptDisplay();
        };

        ////////////////////////////////////////////////////////////////////////
        // Handle setting focus
        self.container.click(function(){
        	self.typer.focus();
            inner.addClass('jquery-console-focus');
            inner.removeClass('jquery-console-nofocus');
            scrollToBottom();
            return false;
        });
		
        // Scroll to the bottom of the view
		// FIXME: This is borked on Chrome.
        function scrollToBottom () {
			//target.offsetParent().scrollTop(target.offset().top - 50);
			self.container.attr({scrollTop: self.container.attr("scrollHeight")});
        }
		
        ////////////////////////////////////////////////////////////////////////
        // Handle losing focus
        self.typer.blur(function(){
            inner.removeClass('jquery-console-focus');
            inner.addClass('jquery-console-nofocus');
        });

        ////////////////////////////////////////////////////////////////////////
        // Handle key hit before translation
        // For picking up control characters like up/left/down/right

        self.typer.keydown(function(e){
            cancelKeyPress = 0;
			self.consoleControl(e);
        });
        
        ////////////////////////////////////////////////////////////////////////
        // Handle key press
        self.typer.keypress(function(e){
            var keyCode = e.keyCode || e.which;
            if (cancelKeyPress != keyCode && keyCode >= 32){
                if (cancelKeyPress) return false;
                if (typeof config.charInsertTrigger == 'undefined' ||
                    (typeof config.charInsertTrigger == 'function' &&
                     config.charInsertTrigger(keyCode,self.promptText)))
                    self.typer.consoleInsert(keyCode);
            }
            if ($.browser.webkit) return false;
        });

        // Is a keycode a contorl character? 
        // E.g. up, down, left, right, backspc, return, etc.
        function isControlCharacter (keyCode) {
            // TODO: Make more precise/fast.
            return (
                (keyCode >= self.keyCodes.left && keyCode <= self.keyCodes.down)
                    || keyCode == self.keyCodes.back || keyCode == self.keyCodes.del
                    || keyCode == self.keyCodes.end || keyCode == self.keyCodes.start
                    || keyCode == self.keyCodes.ret
            );
        };
		
		self.consoleControl = function (e) {
			return self.defaultConsoleControl(e);
		};
		
        ////////////////////////////////////////////////////////////////////////
        // Handle console control keys
        // E.g. up, down, left, right, backspc, return, etc.
		
		self.cancelKey = function (keyCode) {
			cancelKeyPress = keyCode;
		};
		
        self.defaultConsoleControl = function (e) {
            switch (e.keyCode) {
            case self.keyCodes.left:{
				self.cancelKey(e.keyCode);
                moveColumn(-1);
                updatePromptDisplay(); 
                return false;
                break;
            }
            case self.keyCodes.right:{
				self.cancelKey(e.keyCode);
                moveColumn(1); 
                updatePromptDisplay();
                return false;
                break; 
            }
            case self.keyCodes.back:{
				self.cancelKey(e.keyCode);
                if (moveColumn(-1)){
                    deleteCharAtPos();
                    updatePromptDisplay();
                }
                return false;
                break;
            }
            case self.keyCodes.del:{
				self.cancelKey(e.keyCode);
                if (deleteCharAtPos())
                    updatePromptDisplay();
                return false;
                break;
            }
            case self.keyCodes.end:{
				self.cancelKey(e.keyCode);
				self.moveToEnd();
                return false;
                break;
            }
            case self.keyCodes.start:{
				self.cancelKey(e.keyCode);
				self.moveToStart();
                return false;
                break;
            }
            case self.keyCodes.ret:{
				self.cancelKey(e.keyCode);
                commandTrigger(); return false;
            }
            case self.keyCodes.up:{
				self.cancelKey(e.keyCode);
                rotateHistory(-1); return false;
            }
            case self.keyCodes.down:{
				self.cancelKey(e.keyCode);
                rotateHistory(1); return false;
            }
            default: //alert("Unknown control character: " + keyCode);
            }
        };
		
		self.moveToStart = function () {
            if (moveColumn(-self.column))
                updatePromptDisplay();
		};
		
		self.moveToEnd = function () {
            if (moveColumn(self.promptText.length-self.column))
                updatePromptDisplay();
		};
		
		self.moveWordLeft = function () {
			var c = self.column;
			var s = self.promptText;
			var match = s.slice(0, c).match(/(^|\w+)\W*$/);
			if (match && match.length >= 1) {
				c -= match[0].length;
				self.column = c;
				updatePromptDisplay();
			}
		};
		
		self.moveWordRight = function () {
			var c = self.column;
			var s = self.promptText;
			var match = s.slice(c).match(/^\W*(\w+|$)/);
			if (match && match.length >= 1) {
				c += match[0].length;
				self.column = c;
				updatePromptDisplay();
			}
		};
		
		self.deleteWordLeft = function () {
			var c = self.column;
			var s = self.promptText;
			self.moveWordLeft();
			self.promptText = s.slice(0, self.column) + s.slice(c);
			updatePromptDisplay();
		};
		
		self.deleteWordRight = function () {
			var c = self.column;
			var s = self.promptText;
			self.moveWordRight();
			self.promptText = s.slice(0, c) + s.slice(self.column);
			self.column = c;
			updatePromptDisplay();
		};
		
        ////////////////////////////////////////////////////////////////////////
        // Rotate through the command history
		function rotateHistory (n) {
			if (self.history.length == 0) {
				return;
			}
            
			self.historyCursor += n;
			
			if (self.historyCursor < 0) {
				self.historyCursor = -1;
			} else if (self.historyCursor > self.history.length) {
				self.historyCursor = self.history.length;
			}
			
			if (self.historyCursor == self.history.length ||
				self.historyCursor == -1) {
				self.promptText = '';
			} else {
				self.promptText = self.history[self.historyCursor];
			}
			self.column = self.promptText.length;
			
            updatePromptDisplay();
        };

		/**
		 * Adds test to the history and resets the history cursor to
		 * the end.
 		 */
		function addToHistory (text){
			self.history.push(text);
			self.historyCursor = self.history.length;
			self.restoreText = '';
		}

        // Delete the character at the current position
        function deleteCharAtPos(){
            if (self.promptText != ''){
                self.promptText =
                    self.promptText.substring(0,self.column) +
                    self.promptText.substring(self.column+1);
                self.restoreText = self.promptText;
                return true;
            } else return false;
        };
		
		/**
		 * Validate command and trigger it if valid, or show a validation error
		 */
		function commandTrigger () {
			var line = self.promptText;
			if (typeof config.commandValidate == 'function') {
				var ret = config.commandValidate(line);
				if (ret == true || ret == false) {
					if (ret) {
						handleCommand();
					}
				} else {
					self.commandResult(ret, "jquery-console-message-error");
				}
			} else {
				handleCommand();
			}
		}
		
        ////////////////////////////////////////////////////////////////////////
        // Handle a command
        function handleCommand () {
            if (typeof config.commandHandle == 'function') {
				var text = self.multiLineCommand ? self.currentText + '\n' + self.promptText
					: self.promptText;
                var ret = config.commandHandle(text, function(msgs) {
                    self.commandResult(msgs);
                });
				if (ret === null) { // This command needs more text
					self.multiLineCommand = true;
					self.currentText = text;
		            self.column = -1;
		            updatePromptDisplay();
		            self.newPromptBox();
				} else {
					if (typeof ret == 'boolean') {
	                    if (ret) {
	                        // Command succeeded without a result.
	                        addToHistory(text);
	                        self.commandResult();
	                    } else {
	                        addToHistory(text);
	                        self.commandResult('Command failed.',
	 							"jquery-console-message-error");
	                    }
	                } else if (typeof ret == "string") {
	                    addToHistory(text);
	                    self.commandResult(ret, "jquery-console-message-success");
	                } else if (typeof ret == 'undefined') {
	                    addToHistory(text);
	                } else if (ret.length) {
	                    addToHistory(text);
	                    self.commandResult(ret);
	                }
				}
			}
        }

        ////////////////////////////////////////////////////////////////////////
        // Reset the prompt in invalid command
        self.commandResult = function (msg,className) {
			self.multiLineCommand = false;
			self.currentText = "";
            self.column = -1;
            updatePromptDisplay();
            if (typeof msg == 'string') {
                self.message(msg,className);
            } else {
                for (var x in msg) {
                    var ret = msg[x];
                    self.message(self.ps3 + ret.msg, ret.className);
                }
            }
            self.newPromptBox();
        };

        ////////////////////////////////////////////////////////////////////////
        // Display a message
        self.message = function (msg,className) {
            var mesg = $('<div class="jquery-console-message"></div>');
            if (className) mesg.addClass(className);
            mesg.filledText(msg).hide();
            inner.append(mesg);
            mesg.show();
        };

        ////////////////////////////////////////////////////////////////////////
        // Handle normal character insertion
        self.typer.consoleInsert = function(keyCode){
            // TODO: remove redundant indirection
            var char = String.fromCharCode(keyCode);
            var before = self.promptText.substring(0,self.column);
            var after = self.promptText.substring(self.column);
            self.promptText = before + char + after;
            moveColumn(1);
            self.restoreText = self.promptText;
            updatePromptDisplay();
        };
        
        ////////////////////////////////////////////////////////////////////////
        // Move to another column relative to this one
        // Negative means go back, positive means go forward.
        function moveColumn(n){
            if (self.column + n >= 0 && self.column + n <= self.promptText.length){
                self.column += n;
                return true;
            } else return false;
        };

        self.setPromptText = function(text){
            if (text) {
                self.promptText = text;
                if (self.column > self.promptText.length)
                    self.column = self.promptText.length;
                updatePromptDisplay();
            }
            return self.promptText;
        };

        ////////////////////////////////////////////////////////////////////////
        // Update the prompt display
        function updatePromptDisplay () {
            var line = self.promptText;
            var html = '';
            if (self.column > 0 && line == ''){
                // When we have an empty line just display a cursor.
                html = self.cursor;
            } else if (self.column == self.promptText.length){
                // We're at the end of the line, so we need to display
                // the text *and* cursor.
                html = htmlEncode(line) + self.cursor;
            } else {
                // Grab the current character, if there is one, and
                // make it the current cursor.
                var before = line.substring(0, self.column);
                var current = line.substring(self.column,self.column+1);
                if (current){
                    current = 
                        '<span class="jquery-console-cursor">' +
                        htmlEncode(current) +
                        '</span>';
                }
                var after = line.substring(self.column+1);
                html = htmlEncode(before) + current + htmlEncode(after);
            }
            self.prompt.html(html);
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
            self.container.append(inner);
            inner.append(self.typer);
            self.typer.css({position:'absolute',top:0,left:'-9999px'});
            if (config.welcomeMessage)
                self.message(config.welcomeMessage,'jquery-console-welcome');
            self.newPromptBox();
            if (config.autofocus) {
                inner.addClass('jquery-console-focus');
                self.typer.focus();
                setTimeout(function(){
                    inner.addClass('jquery-console-focus');
                    self.typer.focus();
                },100);
            }
            self.inner = inner;
            self.scrollToBottom = scrollToBottom;
        })();
		
        return self;
    };
    // Simple utility for printing messages
    $.fn.filledText = function(txt){
        $(this).text(txt);
        $(this).html($(this).html().replace(/\n/g,'<br/>'));
        return this;
    };
})(jQuery);
