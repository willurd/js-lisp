// ================================================================================
// js-lisp: A, 100% JavaScript, lisp interpreter for browser scripting
// 
// Disclaimer: DO NOT attempt to use this project for any production code
// whatsoever. This project is still VERY young; if there were such a thing
// as negative version numbers, it would have one. For now, this is a toy
// project. It is my hope that js-lisp will one day grow up to become a solid
// library devs around the world can use to sefely get their Lisp fix while
// they get their JavaScript fix, but until that time, DO NOT use this in
// production code (and if you do, I'm not responsible for the craziness that
// will most certainly ensue thereafter).
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// 
// Author: William Bowers <william.bowers@gmail.com>
// ================================================================================

var lisp = (function (global) {
	var jseval = window.eval;
