!function(){"use strict";let n={};(function(){!function(n){function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,(function(r){return function(t){return n(r,t)}}))}function e(n){return r(3,n,(function(r){return function(t){return function(e){return n(r,t,e)}}}))}function u(n){return r(4,n,(function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}}))}function o(n){return r(5,n,(function(r){return function(t){return function(e){return function(u){return function(o){return n(r,t,e,u,o)}}}}}))}function i(n){return r(6,n,(function(r){return function(t){return function(e){return function(u){return function(o){return function(i){return n(r,t,e,u,o,i)}}}}}}))}function a(n){return r(7,n,(function(r){return function(t){return function(e){return function(u){return function(o){return function(i){return function(a){return n(r,t,e,u,o,i,a)}}}}}}}))}function f(n){return r(8,n,(function(r){return function(t){return function(e){return function(u){return function(o){return function(i){return function(a){return function(f){return n(r,t,e,u,o,i,a,f)}}}}}}}}))}function c(n){return r(9,n,(function(r){return function(t){return function(e){return function(u){return function(o){return function(i){return function(a){return function(f){return function(c){return n(r,t,e,u,o,i,a,f,c)}}}}}}}}}))}function s(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function l(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function v(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function d(n,r,t,e,u,o){return 5===n.a?n.f(r,t,e,u,o):n(r)(t)(e)(u)(o)}function h(n,r,t,e,u,o,i){return 6===n.a?n.f(r,t,e,u,o,i):n(r)(t)(e)(u)(o)(i)}function b(n,r){for(var t,e=[],u=p(n,r,0,e);u&&(t=e.pop());u=p(t.a,t.b,0,e));return u}function p(n,r,t,e){if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&I(5),!1;if(t>100)return e.push(y(n,r)),!0;for(var u in"Set_elm_builtin"===n.$&&(n=Nr(n),r=Nr(r)),"RBNode_elm_builtin"!==n.$&&"RBEmpty_elm_builtin"!==n.$||(n=Lr(n),r=Lr(r)),n)if(!p(n[u],r[u],t+1,e))return!1;return!0}console.warn("Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.");t(b),t((function(n,r){return!b(n,r)}));function g(n,r,t){if("object"!=typeof n)return n===r?0:n<r?-1:1;if(n instanceof String){var e=n.valueOf(),u=r.valueOf();return e===u?0:e<u?-1:1}if("#"===n.$[0])return(t=g(n.a,r.a))?t:(t=g(n.b,r.b))?t:g(n.c,r.c);for(;n.b&&r.b&&!(t=g(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}t((function(n,r){return g(n,r)<0})),t((function(n,r){return g(n,r)<1})),t((function(n,r){return g(n,r)>0})),t((function(n,r){return g(n,r)>=0})),t((function(n,r){var t=g(n,r);return t<0?Ar:t?Er:_r}));var m={$:"#0"};function y(n,r){return{$:"#2",a:n,b:r}}function w(n){return new String(n)}function $(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}t(k);function k(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=E(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=E(n.a,r);return t}var _={$:"[]"};function E(n,r){return{$:"::",a:n,b:r}}var A=t(E);function j(n){for(var r=_,t=n.length;t--;)r=E(n[t],r);return r}function T(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var L=e((function(n,r,t){for(var e=[];r.b&&t.b;r=r.b,t=t.b)e.push(s(n,r.a,t.a));return j(e)}));u((function(n,r,t,e){for(var u=[];r.b&&t.b&&e.b;r=r.b,t=t.b,e=e.b)u.push(l(n,r.a,t.a,e.a));return j(u)})),o((function(n,r,t,e,u){for(var o=[];r.b&&t.b&&e.b&&u.b;r=r.b,t=t.b,e=e.b,u=u.b)o.push(v(n,r.a,t.a,e.a,u.a));return j(o)})),i((function(n,r,t,e,u,o){for(var i=[];r.b&&t.b&&e.b&&u.b&&o.b;r=r.b,t=t.b,e=e.b,u=u.b,o=o.b)i.push(d(n,r.a,t.a,e.a,u.a,o.a));return j(i)})),t((function(n,r){return j(T(r).sort((function(r,t){return g(n(r),n(t))})))})),t((function(n,r){return j(T(r).sort((function(r,t){var e=s(n,r,t);return e===_r?0:e===Ar?-1:1})))}));var N=e((function(n,r,t){for(var e=new Array(n),u=0;u<n;u++)e[u]=t(r+u);return e})),O=t((function(n,r){for(var t=new Array(n),e=0;e<n&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,y(t,r)})),C=(t((function(n,r){return r[n]})),e((function(n,r,t){for(var e=t.length,u=new Array(e),o=0;o<e;o++)u[o]=t[o];return u[n]=r,u})),t((function(n,r){for(var t=r.length,e=new Array(t+1),u=0;u<t;u++)e[u]=r[u];return e[t]=n,e})),e((function(n,r,t){for(var e=t.length,u=0;u<e;u++)r=s(n,t[u],r);return r})),e((function(n,r,t){for(var e=t.length-1;e>=0;e--)r=s(n,t[e],r);return r})));t((function(n,r){for(var t=r.length,e=new Array(t),u=0;u<t;u++)e[u]=n(r[u]);return e})),e((function(n,r,t){for(var e=t.length,u=new Array(e),o=0;o<e;o++)u[o]=s(n,r+o,t[o]);return u})),e((function(n,r,t){return t.slice(n,r)})),e((function(n,r,t){var e=r.length,u=n-e;u>t.length&&(u=t.length);for(var o=new Array(e+u),i=0;i<e;i++)o[i]=r[i];for(i=0;i<u;i++)o[i+e]=t[i];return o})),t((function(n,r){return r})),t((function(n,r){return console.log(n+": "+x(r)),r}));function x(n){return function n(r,t){if("function"==typeof t)return F(r,"<function>");if("boolean"==typeof t)return P(r,t?"True":"False");if("number"==typeof t)return function(n,r){return n?"[95m"+r+"[0m":r}(r,t+"");if(t instanceof String)return function(n,r){return n?"[92m"+r+"[0m":r}(r,"'"+S(t,!0)+"'");if("string"==typeof t)return B(r,'"'+S(t,!1)+'"');if("object"==typeof t&&"$"in t){var e=t.$;if("number"==typeof e)return F(r,"<internals>");if("#"===e[0]){var u=[];for(var o in t)"$"!==o&&u.push(n(r,t[o]));return"("+u.join(",")+")"}if("Set_elm_builtin"===e)return P(r,"Set")+D(r,".fromList")+" "+n(r,Nr(t));if("RBNode_elm_builtin"===e||"RBEmpty_elm_builtin"===e)return P(r,"Dict")+D(r,".fromList")+" "+n(r,Lr(t));if("Array_elm_builtin"===e)return P(r,"Array")+D(r,".fromList")+" "+n(r,xr(t));if("::"===e||"[]"===e){u="[";for(t.b&&(u+=n(r,t.a),t=t.b);t.b;t=t.b)u+=","+n(r,t.a);return u+"]"}u="";for(var i in t)if("$"!==i){var a=n(r,t[i]),f=a[0],c="{"===f||"("===f||"["===f||"<"===f||'"'===f||a.indexOf(" ")<0;u+=" "+(c?a:"("+a+")")}return P(r,e)+u}if("function"==typeof DataView&&t instanceof DataView)return B(r,"<"+t.byteLength+" bytes>");if("undefined"!=typeof File&&t instanceof File)return F(r,"<"+t.name+">");if("object"==typeof t){u=[];for(var s in t){var l="_"===s[0]?s.slice(1):s;u.push(D(r,l)+" = "+n(r,t[s]))}return 0===u.length?"{}":"{ "+u.join(", ")+" }"}return F(r,"<internals>")}(!1,n)}function S(n,r){var t=n.replace(/\\/g,"\\\\").replace(/\n/g,"\\n").replace(/\t/g,"\\t").replace(/\r/g,"\\r").replace(/\v/g,"\\v").replace(/\0/g,"\\0");return r?t.replace(/\'/g,"\\'"):t.replace(/\"/g,'\\"')}function P(n,r){return n?"[96m"+r+"[0m":r}function B(n,r){return n?"[93m"+r+"[0m":r}function D(n,r){return n?"[37m"+r+"[0m":r}function F(n,r){return n?"[36m"+r+"[0m":r}function I(n,r,t,e,u){switch(n){case 0:throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');case 1:throw new Error("Browser.application programs cannot handle URLs like this:\n\n    "+document.location.href+"\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.");case 2:throw new Error("Problem with the flags given to your Elm program on initialization.\n\n"+r);case 3:var o=r;throw new Error("There can only be one port named `"+o+"`, but your program has multiple.");case 4:o=r;throw new Error("Trying to send an unexpected type of value through port `"+o+"`:\n"+t);case 5:throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');case 6:var i=r;throw new Error("Your page is loading multiple Elm scripts with a module named "+i+". Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!");case 8:i=r;var a=t,f=e;throw new Error("TODO in module `"+i+"` "+R(a)+"\n\n"+f);case 9:i=r,a=t;var c=e;f=u;throw new Error("TODO in module `"+i+"` from the `case` expression "+R(a)+"\n\nIt received the following value:\n\n    "+x(c).replace("\n","\n    ")+"\n\nBut the branch that handles it says:\n\n    "+f.replace("\n","\n    "));case 10:throw new Error("Bug in https://github.com/elm/virtual-dom/issues");case 11:throw new Error("Cannot perform mod 0. Division by zero error.")}}function R(n){return n.start.line===n.end.line?"on line "+n.start.line:"on lines "+n.start.line+" through "+n.end.line}t((function(n,r){return n+r})),t((function(n,r){return n-r})),t((function(n,r){return n*r})),t((function(n,r){return n/r})),t((function(n,r){return n/r|0})),t(Math.pow),t((function(n,r){return r%n})),t((function(n,r){var t=r%n;return 0===n?I(11):t>0&&n<0||t<0&&n>0?t+n:t})),t(Math.atan2);var z=Math.ceil,q=Math.floor,U=Math.log;t((function(n,r){return n&&r})),t((function(n,r){return n||r})),t((function(n,r){return n!==r})),t((function(n,r){return n+r}));t((function(n,r){return n+r}));t((function(n,r){for(var t=r.length,e=new Array(t),u=0;u<t;){var o=r.charCodeAt(u);55296<=o&&o<=56319?(e[u]=n(w(r[u]+r[u+1])),u+=2):(e[u]=n(w(r[u])),u++)}return e.join("")})),t((function(n,r){for(var t=[],e=r.length,u=0;u<e;){var o=r[u],i=r.charCodeAt(u);u++,55296<=i&&i<=56319&&(o+=r[u],u++),n(w(o))&&t.push(o)}return t.join("")})),e((function(n,r,t){for(var e=t.length,u=0;u<e;){var o=t[u],i=t.charCodeAt(u);u++,55296<=i&&i<=56319&&(o+=t[u],u++),r=s(n,w(o),r)}return r})),e((function(n,r,t){for(var e=t.length;e--;){var u=t[e],o=t.charCodeAt(e);56320<=o&&o<=57343&&(u=t[--e]+u),r=s(n,w(u),r)}return r}));var J=t((function(n,r){return r.split(n)})),M=t((function(n,r){return r.join(n)})),W=e((function(n,r,t){return t.slice(n,r)})),G=(t((function(n,r){for(var t=r.length;t--;){var e=r[t],u=r.charCodeAt(t);if(56320<=u&&u<=57343&&(e=r[--t]+e),n(w(e)))return!0}return!1})),t((function(n,r){for(var t=r.length;t--;){var e=r[t],u=r.charCodeAt(t);if(56320<=u&&u<=57343&&(e=r[--t]+e),!n(w(e)))return!1}return!0}))),Y=t((function(n,r){return r.indexOf(n)>-1})),H=t((function(n,r){return 0===r.indexOf(n)})),K=(t((function(n,r){return r.length>=n.length&&r.lastIndexOf(n)===r.length-n.length})),t((function(n,r){var t=n.length;if(t<1)return _;for(var e=0,u=[];(e=r.indexOf(n,e))>-1;)u.push(e),e+=t;return j(u)})));t((function(n,r){return{$:6,d:n,b:r}})),t((function(n,r){return{$:7,e:n,b:r}}));function V(n,r){return{$:9,f:n,g:r}}t((function(n,r){return{$:10,b:r,h:n}}));var Q=t((function(n,r){return V(n,[r])})),X=e((function(n,r,t){return V(n,[r,t])})),Z=(u((function(n,r,t,e){return V(n,[r,t,e])})),o((function(n,r,t,e,u){return V(n,[r,t,e,u])})),i((function(n,r,t,e,u,o){return V(n,[r,t,e,u,o])})),a((function(n,r,t,e,u,o,i){return V(n,[r,t,e,u,o,i])})),f((function(n,r,t,e,u,o,i,a){return V(n,[r,t,e,u,o,i,a])})),c((function(n,r,t,e,u,o,i,a,f){return V(n,[r,t,e,u,o,i,a,f])})),t((function(n,r){try{return nn(n,JSON.parse(r))}catch(n){return Sr(s(Pr,"This is not valid JSON! "+n.message,cn(r)))}})),t((function(n,r){return nn(n,sn(r))})));function nn(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?Fr(n.c):un("null",r);case 3:return tn(r)?rn(n.b,r,j):un("a LIST",r);case 4:return tn(r)?rn(n.b,r,en):un("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return un("an OBJECT with a field named `"+t+"`",r);var e=nn(n.b,r[t]);return At(e)?e:Sr(s(Br,t,e.a));case 7:var u=n.e;if(!tn(r))return un("an ARRAY",r);if(u>=r.length)return un("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=nn(n.b,r[u]);return At(e)?e:Sr(s(Dr,u,e.a));case 8:if("object"!=typeof r||null===r||tn(r))return un("an OBJECT",r);var o=_;for(var i in r)if(r.hasOwnProperty(i)){e=nn(n.b,r[i]);if(!At(e))return Sr(s(Br,i,e.a));o=E(y(i,e.a),o)}return Fr(ut(o));case 9:for(var a=n.f,f=n.g,c=0;c<f.length;c++){e=nn(f[c],r);if(!At(e))return e;a=a(e.a)}return Fr(a);case 10:e=nn(n.b,r);return At(e)?nn(n.h(e.a),r):e;case 11:for(var l=_,v=n.g;v.b;v=v.b){e=nn(v.a,r);if(At(e))return e;l=E(e.a,l)}return Sr(Ir(ut(l)));case 1:return Sr(s(Pr,n.a,cn(r)));case 0:return Fr(n.a)}}function rn(n,r,t){for(var e=r.length,u=new Array(e),o=0;o<e;o++){var i=nn(n,r[o]);if(!At(i))return Sr(s(Dr,o,i.a));u[o]=i.a}return Fr(t(u))}function tn(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function en(n){return s(Et,n.length,(function(r){return n[r]}))}function un(n,r){return Sr(s(Pr,"Expecting "+n,cn(r)))}function on(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return on(n.b,r.b);case 6:return n.d===r.d&&on(n.b,r.b);case 7:return n.e===r.e&&on(n.b,r.b);case 9:return n.f===r.f&&an(n.g,r.g);case 10:return n.h===r.h&&on(n.b,r.b);case 11:return an(n.g,r.g)}}function an(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;e<t;e++)if(!on(n[e],r[e]))return!1;return!0}var fn=t((function(n,r){return JSON.stringify(sn(r),null,n)+""}));function cn(n){return{$:0,a:n}}function sn(n){return n.a}e((function(n,r,t){return t[n]=sn(r),t}));function ln(n){return{$:0,a:n}}function vn(n){return{$:2,b:n,c:null}}var dn=t((function(n,r){return{$:3,b:n,d:r}}));t((function(n,r){return{$:4,b:n,d:r}}));var hn=0;function bn(n){var r={$:0,e:hn++,f:n,g:null,h:[]};return $n(r),r}function pn(n){return vn((function(r){r(ln(bn(n)))}))}function gn(n,r){n.h.push(r),$n(n)}var mn=t((function(n,r){return vn((function(t){gn(n,r),t(ln(m))}))})),yn=!1,wn=[];function $n(n){if(wn.push(n),!yn){for(yn=!0;n=wn.shift();)kn(n);yn=!1}}function kn(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b((function(r){n.f=r,$n(n)})));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}u((function(n,r,t,e){return _n(r,e,n.init,n.update,n.subscriptions,(function(){return function(){}}))}));function _n(n,r,t,e,u,o){var i,a=s(Z,n,cn(r?r.flags:void 0));At(a)||I(2,(i=a.a,at(i)));var f={},c=(a=t(a.a)).a,l=o(d,c),v=function(n,r){var t;for(var e in En){var u=En[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=An(u,r)}return t}(f,d);function d(n,r){a=s(e,n,c),l(c=a.a,r),On(f,a.b,u(c))}return On(f,a.b,u(c)),v?{ports:v}:{}}var En={};function An(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,o=n.e,i=n.f;return t.h=bn(s(dn,(function n(r){return s(dn,n,{$:5,b:function(n){var a=n.a;return 0===n.$?l(u,t,a,r):o&&i?v(e,t,a.i,a.j,r):l(e,t,o?a.i:a.j,r)}})}),n.b))}var jn=t((function(n,r){return vn((function(t){n.g(r),t(ln(m))}))}));t((function(n,r){return s(mn,n.h,{$:0,a:r})}));function Tn(n){return{$:2,m:n}}t((function(n,r){return{$:3,n:n,o:r}}));var Ln=[],Nn=!1;function On(n,r,t){if(Ln.push({p:n,q:r,r:t}),!Nn){Nn=!0;for(var e;e=Ln.shift();)Cn(e.p,e.q,e.r);Nn=!1}}function Cn(n,r,t){var e={};for(var u in xn(!0,r,e,null),xn(!1,t,e,null),n)gn(n[u],{$:"fx",a:e[u]||{i:_,j:_}})}function xn(n,r,t,e){switch(r.$){case 1:var u=r.k,o=function(n,r,t,e){return s(n?En[r].e:En[r].f,(function(n){for(var r=t;r;r=r.t)n=r.s(n);return n}),e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:_,j:_},n?t.i=E(r,t.i):t.j=E(r,t.j),t}(n,o,t[u]));case 2:for(var i=r.m;i.b;i=i.b)xn(n,i.a,t,e);return;case 3:return void xn(n,r.o,t,{s:r.n,t:e})}}var Sn;t((function(n,r){return r})),t((function(n,r){return function(t){return n(r(t))}}));var Pn="undefined"!=typeof document?document:{};function Bn(n,r){n.appendChild(r)}u((function(n,r,t,e){var u=e&&e.node?e.node:I(0);return u.parentNode.replaceChild(Yn(n,(function(){})),u),{}}));function Dn(n){return{$:0,a:n}}var Fn=t((function(n,r){return t((function(t,e){for(var u=[],o=0;e.b;e=e.b){var i=e.a;o+=i.b||0,u.push(i)}return o+=u.length,{$:1,c:r,d:Wn(t),e:u,f:n,b:o}}))}))(void 0);t((function(n,r){return t((function(t,e){for(var u=[],o=0;e.b;e=e.b){var i=e.a;o+=i.b.b||0,u.push(i)}return o+=u.length,{$:2,c:r,d:Wn(t),e:u,f:n,b:o}}))}))(void 0),t((function(n,r){return{$:4,j:n,k:r,b:1+(r.b||0)}}));function In(n,r){return{$:5,l:n,m:r,k:void 0}}t((function(n,r){return In([n,r],(function(){return n(r)}))})),e((function(n,r,t){return In([n,r,t],(function(){return s(n,r,t)}))})),u((function(n,r,t,e){return In([n,r,t,e],(function(){return l(n,r,t,e)}))})),o((function(n,r,t,e,u){return In([n,r,t,e,u],(function(){return v(n,r,t,e,u)}))})),i((function(n,r,t,e,u,o){return In([n,r,t,e,u,o],(function(){return d(n,r,t,e,u,o)}))})),a((function(n,r,t,e,u,o,i){return In([n,r,t,e,u,o,i],(function(){return h(n,r,t,e,u,o,i)}))})),f((function(n,r,t,e,u,o,i,a){return In([n,r,t,e,u,o,i,a],(function(){return function(n,r,t,e,u,o,i,a){return 7===n.a?n.f(r,t,e,u,o,i,a):n(r)(t)(e)(u)(o)(i)(a)}(n,r,t,e,u,o,i,a)}))})),c((function(n,r,t,e,u,o,i,a,f){return In([n,r,t,e,u,o,i,a,f],(function(){return function(n,r,t,e,u,o,i,a,f){return 8===n.a?n.f(r,t,e,u,o,i,a,f):n(r)(t)(e)(u)(o)(i)(a)(f)}(n,r,t,e,u,o,i,a,f)}))}));var Rn=t((function(n,r){return{$:"a0",n:n,o:r}})),zn=(t((function(n,r){return{$:"a1",n:n,o:r}})),t((function(n,r){return{$:"a2",n:n,o:r}}))),qn=t((function(n,r){return{$:"a3",n:n,o:r}}));e((function(n,r,t){return{$:"a4",n:r,o:{f:n,o:t}}}));t((function(n,r){return"a0"===r.$?s(Rn,r.n,function(n,r){var t=Nt(r);return{$:r.$,a:t?l(Tt,t<3?Jn:Mn,Lt(n),r.a):s(jt,n,r.a)}}(n,r.o)):r}));var Un,Jn=t((function(n,r){return y(n(r.a),r.b)})),Mn=t((function(n,r){return{message:n(r.message),stopPropagation:r.stopPropagation,preventDefault:r.preventDefault}}));function Wn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,o=t.o;if("a2"!==e){var i=r[e]||(r[e]={});"a3"===e&&"class"===u?Gn(i,u,o):i[u]=o}else"className"===u?Gn(r,u,sn(o)):r[u]=sn(o)}return r}function Gn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Yn(n,r){var t=n.$;if(5===t)return Yn(n.k||(n.k=n.m()),r);if(0===t)return Pn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var o={j:u,p:r};return(i=Yn(e,o)).elm_event_node_ref=o,i}if(3===t)return Hn(i=n.h(n.g),r,n.d),i;var i=n.f?Pn.createElementNS(n.f,n.c):Pn.createElement(n.c);Sn&&"a"==n.c&&i.addEventListener("click",Sn(i)),Hn(i,r,n.d);for(var a=n.e,f=0;f<a.length;f++)Bn(i,Yn(1===t?a[f]:a[f].b,r));return i}function Hn(n,r,t){for(var e in t){var u=t[e];"a1"===e?Kn(n,u):"a0"===e?Xn(n,r,u):"a3"===e?Vn(n,u):"a4"===e?Qn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function Kn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Vn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Qn(n,r){for(var t in r){var e=r[t],u=e.f,o=e.o;void 0!==o?n.setAttributeNS(u,t,o):n.removeAttributeNS(u,t)}}function Xn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var o=t[u],i=e[u];if(o){if(i){if(i.q.$===o.$){i.q=o;continue}n.removeEventListener(u,i)}i=Zn(r,o),n.addEventListener(u,i,Un&&{passive:Nt(o)<2}),e[u]=i}else n.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Un=!0}}))}catch(n){}function Zn(n,r){function t(r){var e=t.q,u=nn(e.a,r);if(At(u)){for(var o,i=Nt(e),a=u.a,f=i?i<3?a.a:a.message:a,c=1==i?a.b:3==i&&a.stopPropagation,s=(c&&r.stopPropagation(),(2==i?a.b:3==i&&a.preventDefault)&&r.preventDefault(),n);o=s.j;){if("function"==typeof o)f=o(f);else for(var l=o.length;l--;)f=o[l](f);s=s.p}s(f,c)}}return t.q=r,t}function nr(n,r){return n.$==r.$&&on(n.a,r.a)}function rr(n,r){var t=[];return er(n,r,t,0),t}function tr(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function er(n,r,t,e){if(n!==r){var u=n.$,o=r.$;if(u!==o){if(1!==u||2!==o)return void tr(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=new Array(t),u=0;u<t;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),o=1}switch(o){case 5:for(var i=n.l,a=r.l,f=i.length,c=f===a.length;c&&f--;)c=i[f]===a[f];if(c)return void(r.k=n.k);r.k=r.m();var s=[];return er(n.k,r.k,s,0),void(s.length>0&&tr(t,1,e,s));case 4:for(var l=n.j,v=r.j,d=!1,h=n.k;4===h.$;)d=!0,"object"!=typeof l?l=[l,h.j]:l.push(h.j),h=h.k;for(var b=r.k;4===b.$;)d=!0,"object"!=typeof v?v=[v,b.j]:v.push(b.j),b=b.k;return d&&l.length!==v.length?void tr(t,0,e,r):((d?function(n,r){for(var t=0;t<n.length;t++)if(n[t]!==r[t])return!1;return!0}(l,v):l===v)||tr(t,2,e,v),void er(h,b,t,e+1));case 0:return void(n.a!==r.a&&tr(t,3,e,r.a));case 1:return void ur(n,r,t,e,ir);case 2:return void ur(n,r,t,e,ar);case 3:if(n.h!==r.h)return void tr(t,0,e,r);var p=or(n.d,r.d);p&&tr(t,4,e,p);var g=r.i(n.g,r.g);return void(g&&tr(t,5,e,g))}}}function ur(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var o=or(n.d,r.d);o&&tr(t,4,e,o),u(n,r,t,e)}else tr(t,0,e,r)}function or(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var o=n[u],i=r[u];o===i&&"value"!==u&&"checked"!==u||"a0"===t&&nr(o,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var a=or(n[u],r[u]||{},u);a&&((e=e||{})[u]=a)}for(var f in r)f in n||((e=e||{})[f]=r[f]);return e}function ir(n,r,t,e){var u=n.e,o=r.e,i=u.length,a=o.length;i>a?tr(t,6,e,{v:a,i:i-a}):i<a&&tr(t,7,e,{v:i,e:o});for(var f=i<a?i:a,c=0;c<f;c++){var s=u[c];er(s,o[c],t,++e),e+=s.b||0}}function ar(n,r,t,e){for(var u=[],o={},i=[],a=n.e,f=r.e,c=a.length,s=f.length,l=0,v=0,d=e;l<c&&v<s;){var h=a[l],b=f[v],p=h.a,g=b.a,m=h.b,y=b.b,w=void 0,$=void 0;if(p!==g){var k=a[l+1],_=f[v+1];if(k){var E=k.a,A=k.b;$=g===E}if(_){var j=_.a,T=_.b;w=p===j}if(w&&$)er(m,T,u,++d),fr(o,u,p,y,v,i),d+=m.b||0,cr(o,u,p,A,++d),d+=A.b||0,l+=2,v+=2;else if(w)d++,fr(o,u,g,y,v,i),er(m,T,u,d),d+=m.b||0,l+=1,v+=2;else if($)cr(o,u,p,m,++d),d+=m.b||0,er(A,y,u,++d),d+=A.b||0,l+=2,v+=1;else{if(!k||E!==j)break;cr(o,u,p,m,++d),fr(o,u,g,y,v,i),d+=m.b||0,er(A,T,u,++d),d+=A.b||0,l+=2,v+=2}}else er(m,y,u,++d),d+=m.b||0,l++,v++}for(;l<c;){d++;m=(h=a[l]).b;cr(o,u,h.a,m,d),d+=m.b||0,l++}for(;v<s;){var L=L||[];fr(o,u,(b=f[v]).a,b.b,void 0,L),v++}(u.length>0||i.length>0||L)&&tr(t,8,e,{w:u,x:i,y:L})}function fr(n,r,t,e,u,o){var i=n[t];if(!i)return i={c:0,z:e,r:u,s:void 0},o.push({r:u,A:i}),void(n[t]=i);if(1===i.c){o.push({r:u,A:i}),i.c=2;var a=[];return er(i.z,e,a,i.r),i.r=u,void(i.s.s={w:a,A:i})}fr(n,r,t+"_elmW6BL",e,u,o)}function cr(n,r,t,e,u){var o=n[t];if(o){if(0===o.c){o.c=2;var i=[];return er(e,o.z,i,u),void tr(r,9,u,{w:i,A:o})}cr(n,r,t+"_elmW6BL",e,u)}else{var a=tr(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:a}}}function sr(n,r,t,e){!function n(r,t,e,u,o,i,a){var f=e[u],c=f.r;for(;c===o;){var s=f.$;if(1===s)sr(r,t.k,f.s,a);else if(8===s){f.t=r,f.u=a,(l=f.s.w).length>0&&n(r,t,l,0,o,i,a)}else if(9===s){f.t=r,f.u=a;var l,v=f.s;if(v)v.A.s=r,(l=v.w).length>0&&n(r,t,l,0,o,i,a)}else f.t=r,f.u=a;if(u++,!(f=e[u])||(c=f.r)>i)return u}var d=t.$;if(4===d){for(var h=t.k;4===h.$;)h=h.k;return n(r,h,e,u,o+1,i,r.elm_event_node_ref)}for(var b=t.e,p=r.childNodes,g=0;g<b.length;g++){o++;var m=1===d?b[g]:b[g].b,y=o+(m.b||0);if(o<=c&&c<=y&&(u=n(p[g],m,e,u,o,y,a),!(f=e[u])||(c=f.r)>i))return u;o=y}return u}(n,r,t,0,0,r.b,e)}function lr(n,r,t,e){return 0===t.length?n:(sr(n,r,t,e),vr(n,t))}function vr(n,r){for(var t=0;t<r.length;t++){var e=r[t],u=e.t,o=dr(u,e);u===n&&(n=o)}return n}function dr(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=Yn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return Hn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return vr(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;e<t.i;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,o=(e=t.v,n.childNodes[e]);e<u.length;e++)n.insertBefore(Yn(u[e],r.u),o);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var i=t.A;return void 0!==i.r&&n.parentNode.removeChild(n),i.s=vr(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=Pn.createDocumentFragment(),e=0;e<n.length;e++){var u=n[e].A;Bn(t,2===u.c?u.s:Yn(u.z,r.u))}return t}(t.y,r);n=vr(n,t.w);for(var u=t.x,o=0;o<u.length;o++){var i=u[o],a=i.A,f=2===a.c?a.s:Yn(a.z,r.u);n.insertBefore(f,n.childNodes[i.r])}e&&Bn(n,e);return n}(n,r);case 5:return r.s(n);default:I(10)}}function hr(n){if(3===n.nodeType)return Dn(n.textContent);if(1!==n.nodeType)return Dn("");for(var r=_,t=n.attributes,e=t.length;e--;){var u=t[e],o=u.name,i=u.value;r=E(s(qn,o,i),r)}var a=n.tagName.toLowerCase(),f=_,c=n.childNodes;for(e=c.length;e--;)f=E(hr(c[e]),f);return l(Fn,a,r,f)}u((function(n,r,t,e){return _n(r,e,n.init,n.update,n.subscriptions,(function(r,t){var u=n.view,o=e&&e.node?e.node:I(0),i=hr(o);return gr(t,(function(n){var t=u(n),e=rr(i,t);o=lr(o,i,e,r),i=t}))}))}));var br=u((function(n,r,t,e){return _n(r,e,n.init,n.update,n.subscriptions,(function(r,t){var e=n.setup&&n.setup(r),u=n.view,o=Pn.title,i=Pn.body,a=hr(i);return gr(t,(function(n){Sn=e;var t=u(n),f=Fn("body")(_)(t.body),c=rr(a,f);i=lr(i,a,c,r),a=f,Sn=0,o!==t.title&&(Pn.title=o=t.title)}))}))})),pr="undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)};function gr(n,r){r(n);var t=0;function e(){t=1===t?0:(pr(e),r(n),1)}return function(u,o){n=u,o?(r(n),2===t&&(t=1)):(0===t&&pr(e),t=2)}}function mr(){return Kt(Pn.location.href).a||I(1)}t((function(n,r){return s(he,Vt,vn((function(){r&&history.go(r),n()})))}));var yr=t((function(n,r){return s(he,Vt,vn((function(){history.pushState({},"",r),n()})))})),wr=(t((function(n,r){return s(he,Vt,vn((function(){history.replaceState({},"",r),n()})))})),"undefined"!=typeof window?window:{addEventListener:function(){},removeEventListener:function(){}});e((function(n,r,t){return pn(vn((function(e){function u(n){bn(t(n))}return n.addEventListener(r,u,Un&&{passive:!0}),function(){n.removeEventListener(r,u)}})))})),t((function(n,r){var t=nn(n,r);return At(t)?Rr(t.a):zr}));function $r(n,r){return vn((function(t){pr((function(){var e=document.getElementById(n);t(e?ln(r(e)):{$:1,a:xt(n)})}))}))}t((function(n,r){return $r(r,(function(r){return r[n](),m}))})),t((function(n,r){return t=function(){return wr.scroll(n,r),m},vn((function(n){pr((function(){n(ln(t()))}))}));var t})),e((function(n,r,t){return $r(n,(function(n){return n.scrollLeft=r,n.scrollTop=t,m}))}));var kr,_r={$:"EQ"},Er={$:"GT"},Ar={$:"LT"},jr=A,Tr=e((function(n,r,t){for(;;){if("RBEmpty_elm_builtin"===t.$)return r;var e=t.b,u=t.c,o=t.d,i=t.e,a=n,f=l(n,e,u,l(Tr,n,r,i));n=a,r=f,t=o}})),Lr=function(n){return l(Tr,e((function(n,r,t){return s(jr,y(n,r),t)})),_,n)},Nr=function(n){return function(n){return l(Tr,e((function(n,r,t){return s(jr,n,t)})),_,n)}(n.a)},Or=C,Cr=e((function(n,r,e){var u=e.c,o=e.d,i=t((function(r,t){if("SubTree"===r.$){var e=r.a;return l(Or,i,t,e)}var u=r.a;return l(Or,n,t,u)}));return l(Or,i,l(Or,n,r,o),u)})),xr=function(n){return l(Cr,jr,_,n)},Sr=function(n){return{$:"Err",a:n}},Pr=t((function(n,r){return{$:"Failure",a:n,b:r}})),Br=t((function(n,r){return{$:"Field",a:n,b:r}})),Dr=t((function(n,r){return{$:"Index",a:n,b:r}})),Fr=function(n){return{$:"Ok",a:n}},Ir=function(n){return{$:"OneOf",a:n}},Rr=function(n){return{$:"Just",a:n}},zr={$:"Nothing"},qr=G,Ur=fn,Jr=function(n){return n+""},Mr=t((function(n,r){return s(M,n,T(r))})),Wr=t((function(n,r){return j(s(J,n,r))})),Gr=function(n){return s(Mr,"\n    ",s(Wr,"\n",n))},Yr=e((function(n,r,t){for(;;){if(!t.b)return r;var e=t.a,u=t.b,o=n,i=s(n,e,r);n=o,r=i,t=u}})),Hr=function(n){return l(Yr,t((function(n,r){return r+1})),0,n)},Kr=L,Vr=e((function(n,r,t){for(;;){if(!(g(n,r)<1))return t;var e=n,u=r-1,o=s(jr,r,t);n=e,r=u,t=o}})),Qr=t((function(n,r){return l(Vr,n,r,_)})),Xr=t((function(n,r){return l(Kr,n,s(Qr,0,Hr(r)-1),r)})),Zr=function(n){var r=n.charCodeAt(0);return 55296<=r&&r<=56319?1024*(r-55296)+n.charCodeAt(1)-56320+65536:r},nt=function(n){var r=Zr(n);return 97<=r&&r<=122},rt=function(n){var r=Zr(n);return r<=90&&65<=r},tt=function(n){return nt(n)||rt(n)},et=function(n){return nt(n)||rt(n)||function(n){var r=Zr(n);return r<=57&&48<=r}(n)},ut=function(n){return l(Yr,jr,_,n)},ot=function(n){var r=n.charCodeAt(0);return isNaN(r)?zr:Rr(55296<=r&&r<=56319?y(w(n[0]+n[1]),n.slice(2)):y(w(n[0]),n.slice(1)))},it=t((function(n,r){return"\n\n("+Jr(n+1)+") "+Gr(at(r))})),at=function(n){return s(ft,n,_)},ft=t((function(n,r){n:for(;;)switch(n.$){case"Field":var t=n.a,e=n.b,u=function(){var n=ot(t);if("Nothing"===n.$)return!1;var r=n.a,e=r.a,u=r.b;return tt(e)&&s(qr,et,u)}(),o=e,i=s(jr,u?"."+t:"['"+t+"']",r);n=o,r=i;continue n;case"Index":var a=n.a,f=(e=n.b,"["+Jr(a)+"]");o=e,i=s(jr,f,r);n=o,r=i;continue n;case"OneOf":var c=n.a;if(c.b){if(c.b.b){var l=(r.b?"The Json.Decode.oneOf at json"+s(Mr,"",ut(r)):"Json.Decode.oneOf")+" failed in the following "+Jr(Hr(c))+" ways:";return s(Mr,"\n\n",s(jr,l,s(Xr,it,c)))}n=o=e=c.a,r=i=r;continue n}return"Ran into a Json.Decode.oneOf with no possibilities"+(r.b?" at json"+s(Mr,"",ut(r)):"!");default:var v=n.a,d=n.b;return(l=r.b?"Problem with the value at json"+s(Mr,"",ut(r))+":\n\n    ":"Problem with the given value:\n\n")+(Gr(s(Ur,4,d))+"\n\n")+v}})),ct=u((function(n,r,t,e){return{$:"Array_elm_builtin",a:n,b:r,c:t,d:e}})),st=[],lt=z,vt=t((function(n,r){return U(r)/U(n)})),dt=lt(s(vt,2,32)),ht=v(ct,0,dt,st,st),bt=N,pt=(t((function(n,r){return n(r)})),t((function(n,r){return r(n)})),q),gt=function(n){return n.length},mt=t((function(n,r){return g(n,r)>0?n:r})),yt=O,wt=t((function(n,r){for(;;){var t=s(yt,32,n),e=t.a,u=t.b,o=s(jr,{$:"SubTree",a:e},r);if(!u.b)return ut(o);n=u,r=o}})),$t=t((function(n,r){for(;;){var t=lt(r/32);if(1===t)return s(yt,32,n).a;n=s(wt,n,_),r=t}})),kt=t((function(n,r){if(r.nodeListSize){var t=32*r.nodeListSize,e=pt(s(vt,32,t-1)),u=n?ut(r.nodeList):r.nodeList,o=s($t,u,r.nodeListSize);return v(ct,gt(r.tail)+t,s(mt,5,e*dt),o,r.tail)}return v(ct,gt(r.tail),dt,st,r.tail)})),_t=o((function(n,r,t,e,u){for(;;){if(r<0)return s(kt,!1,{nodeList:e,nodeListSize:t/32|0,tail:u});var o={$:"Leaf",a:l(bt,32,r,n)};n=n,r=r-32,t=t,e=s(jr,o,e),u=u}})),Et=t((function(n,r){if(n<=0)return ht;var t=n%32,e=l(bt,t,n-t,r);return d(_t,r,n-t-32,n,_,e)})),At=function(n){return"Ok"===n.$},jt=Q,Tt=X,Lt=function(n){return{$:0,a:n}},Nt=function(n){switch(n.$){case"Normal":return 0;case"MayStopPropagation":return 1;case"MayPreventDefault":return 2;default:return 3}},Ot=function(n){return{$:"External",a:n}},Ct=function(n){return{$:"Internal",a:n}},xt=function(n){return{$:"NotFound",a:n}},St={$:"Http"},Pt={$:"Https"},Bt=i((function(n,r,t,e,u,o){return{fragment:o,host:r,path:e,port_:t,protocol:n,query:u}})),Dt=Y,Ft=function(n){return n.length},It=W,Rt=t((function(n,r){return n<1?r:l(It,n,Ft(r),r)})),zt=K,qt=function(n){return""===n},Ut=t((function(n,r){return n<1?"":l(It,0,n,r)})),Jt=function(n){for(var r=0,t=n.charCodeAt(0),e=43==t||45==t?1:0,u=e;u<n.length;++u){var o=n.charCodeAt(u);if(o<48||57<o)return zr;r=10*r+o-48}return u==e?zr:Rr(45==t?-r:r)},Mt=o((function(n,r,t,e,u){if(qt(u)||s(Dt,"@",u))return zr;var o=s(zt,":",u);if(o.b){if(o.b.b)return zr;var i=o.a,a=Jt(s(Rt,i+1,u));if("Nothing"===a.$)return zr;var f=a;return Rr(h(Bt,n,s(Ut,i,u),f,r,t,e))}return Rr(h(Bt,n,u,zr,r,t,e))})),Wt=u((function(n,r,t,e){if(qt(e))return zr;var u=s(zt,"/",e);if(u.b){var o=u.a;return d(Mt,n,s(Rt,o,e),r,t,s(Ut,o,e))}return d(Mt,n,"/",r,t,e)})),Gt=e((function(n,r,t){if(qt(t))return zr;var e=s(zt,"?",t);if(e.b){var u=e.a;return v(Wt,n,Rr(s(Rt,u+1,t)),r,s(Ut,u,t))}return v(Wt,n,zr,r,t)})),Yt=t((function(n,r){if(qt(r))return zr;var t=s(zt,"#",r);if(t.b){var e=t.a;return l(Gt,n,Rr(s(Rt,e+1,r)),s(Ut,e,r))}return l(Gt,n,zr,r)})),Ht=H,Kt=function(n){return s(Ht,"http://",n)?s(Yt,St,s(Rt,7,n)):s(Ht,"https://",n)?s(Yt,Pt,s(Rt,8,n)):zr},Vt=function(n){for(;;){n=n.a}},Qt=function(n){return{$:"Perform",a:n}},Xt=ln,Zt=Xt(m),ne=u((function(n,r,t,e){if(e.b){var u=e.a,o=e.b;if(o.b){var i=o.a,a=o.b;if(a.b){var f=a.a,c=a.b;if(c.b){var d=c.a,h=c.b;return s(n,u,s(n,i,s(n,f,s(n,d,t>500?l(Yr,n,r,ut(h)):v(ne,n,r,t+1,h)))))}return s(n,u,s(n,i,s(n,f,r)))}return s(n,u,s(n,i,r))}return s(n,u,r)}return r})),re=e((function(n,r,t){return v(ne,n,r,0,t)})),te=t((function(n,r){return l(re,t((function(r,t){return s(jr,n(r),t)})),_,r)})),ee=dn,ue=t((function(n,r){return s(ee,(function(r){return Xt(n(r))}),r)})),oe=e((function(n,r,t){return s(ee,(function(r){return s(ee,(function(t){return Xt(s(n,r,t))}),t)}),r)})),ie=jn,ae=t((function(n,r){var t=r.a;return pn(s(ee,ie(n),t))})),fe=e((function(n,r,t){return s(ue,(function(n){return m}),(e=s(te,ae(n),r),l(re,oe(jr),Xt(_),e)));var e})),ce=e((function(n,r,t){return Xt(m)})),se=t((function(n,r){var t=r.a;return Qt(s(ue,n,t))}));En.Task={b:Zt,c:fe,d:ce,e:se,f:kr};var le,ve,de=(le="Task",function(n){return{$:1,k:le,l:n}}),he=t((function(n,r){return de(Qt(s(ue,n,r)))})),be=function(n){var r=n.onUrlChange,e=n.onUrlRequest,u=function(){u.a(r(mr()))};return br({setup:function(n){return u.a=n,wr.addEventListener("popstate",u),wr.navigator.userAgent.indexOf("Trident")<0||wr.addEventListener("hashchange",u),t((function(r,t){if(!t.ctrlKey&&!t.metaKey&&!t.shiftKey&&t.button<1&&!r.target&&!r.hasAttribute("download")){t.preventDefault();var u=r.href,o=mr(),i=Kt(u).a;n(e(i&&o.protocol===i.protocol&&o.host===i.host&&o.port_.a===i.port_.a?Ct(i):Ot(u)))}}))},init:function(r){return l(n.init,r,mr(),u)},view:n.view,update:n.update,subscriptions:n.subscriptions})},pe=Tn(_),ge={$:"FrontPage"},me={$:"Privacy"},ye={$:"Terms"},we=function(n){var r=n.query;if("Just"===r.$){var t=r.a;return"terms"===t?ye:"privacy"===t?me:ge}return ge},$e=e((function(n,r,t){return y({key:t,page:we(r)},pe)})),ke=Tn(_),_e=function(n){return s(he,Vt,vn((function(r){try{wr.location=n}catch(n){Pn.location.reload(!1)}})))},Ee=yr,Ae=t((function(n,r){if("Nothing"===n.$)return r;var t=n.a;return r+":"+Jr(t)})),je=e((function(n,r,t){return"Nothing"===r.$?t:k(t,k(n,r.a))})),Te=t((function(n,r){var t=y(n,r);switch(t.a.$){case"ClickedLink":var e=t.a.a;if("Internal"===e.$){var u=e.a;return y($(r,{page:we(u)}),s(Ee,r.key,function(n){var r="Http"===n.protocol.$?"http://":"https://";return l(je,"#",n.fragment,l(je,"?",n.query,k(s(Ae,n.port_,k(r,n.host)),n.path)))}(u)))}u=e.a;return y(r,_e(u));case"ChangedUrl":u=t.a.a;return y($(r,{page:we(u)}),pe);default:return y(r,pe)}})),Le=cn,Ne=t((function(n,r){return s(zn,n,Le(r))})),Oe=Ne("className"),Ce=Fn("div"),xe=Fn("h1"),Se=Fn("p"),Pe=Dn,Be=Fn("a"),De=function(n){return s(Ne,"href",/^javascript:/i.test((r=n).replace(/\s/g,""))?'javascript:alert("This is an XSS vector. Please use ports or web components instead.")':r);var r},Fe=qn("rel"),Ie=Ne("target"),Re=e((function(n,r,t){return s(Be,j([De(r),Fe("noopener noreferrer"),Ie("_blank"),Oe(t)]),j([Pe(n)]))})),ze=Fn("li"),qe=Fn("ul"),Ue=function(n){switch(n.$){case"FrontPage":return j([s(Ce,j([Oe("main content text")]),j([s(xe,_,j([Pe("Front page")])),s(Se,_,j([Pe("I will add the front page later")]))]))]);case"Terms":return j([s(Ce,j([Oe("main content text")]),j([s(xe,_,j([Pe("Terms of use")])),s(Se,_,j([Pe("Please do not do anything illegal or taboo with our service.")]))]))]);default:return j([s(Ce,j([Oe("main content text")]),j([s(xe,_,j([Pe("Privacy policy")])),s(Se,_,j([Pe("Our services collect and store all user data you give us to remember that games and users exist. Examples of data collected include, but are not limited to:")])),s(qe,_,j([s(ze,_,j([Pe("Usernames")])),s(ze,_,j([Pe("Account emails")])),s(ze,_,j([Pe("Display names")])),s(ze,_,j([Pe("User-submitted bios")])),s(ze,_,j([Pe("The games you created")])),s(ze,_,j([Pe("The games you joined")])),s(ze,_,j([Pe("Game names")])),s(ze,_,j([Pe("User-submitted game descriptions")])),s(ze,_,j([Pe("Game passphrases (not encrypted)")])),s(ze,_,j([Pe("Whether you were killed in a game")]))])),s(Se,_,j([Pe("We also collect user passwords, but we "),l(Re,"salt","https://en.wikipedia.org/wiki/Salt","link"),Pe(" and encrypt them before storing them. Game passphrases are NOT encrypted so that game creators can return to their game settings page to see what it is.")])),s(Se,_,j([Pe("Only usernames, user display names, user bios, game names, game descriptions, the games you created and joined, and other game-related personal information are publicized.")])),s(Se,_,j([Pe("User information is stored in an unencrypted "),l(Re,"lowdb","https://github.com/typicode/lowdb","link"),Pe(" database without backups on a laptop. This means that someone with access to our database will be able to access a few other data such as everyone's encrypted passwords. We are not responsible for spontaneous data loss. We do not sell your user information because we don't know to whom it should be sold.")])),s(Se,_,j([Pe("If you would like to download or delete all your data, please email "),s(Be,j([De("mailto:sy24484@pausd.us"),Oe("link")]),j([Pe("sy24484@pausd.us")])),Pe(". This is not because we want to discourage users from doing this; rather, we have not gotten to implementing an automatic and more user-friendly method yet.")]))]))])}},Je=Fn("footer"),Me=Fn("span"),We=Fn("header"),Ge=function(n){switch(n.$){case"FrontPage":return"";case"Terms":return"Terms of use";default:return"Privacy policy"}},Ye=be({init:$e,onUrlChange:function(n){return{$:"ChangedUrl",a:n}},onUrlRequest:function(n){return{$:"ClickedLink",a:n}},subscriptions:function(n){return ke},update:Te,view:function(n){return{body:k(j([s(We,j([Oe("header")]),j([s(Be,j([De("?"),Oe("site-name link")]),j([Pe("Elimination")])),s(Me,j([Oe("flex")]),_)]))]),k(Ue(n.page),j([s(Je,j([Oe("footer")]),j([s(Me,_,j([Pe("Created by the creators of "),s(Be,j([De("https://gunn.app/"),Fe("noopener noreferrer"),Ie("_blank"),Oe("link")]),j([Pe("UGWA")])),Pe(".")])),s(Me,j([Oe("flex")]),_),s(Me,_,j([s(Be,j([De("?about"),Oe("link")]),j([Pe("About")])),Pe(" · "),s(Be,j([De("?privacy"),Oe("link")]),j([Pe("Privacy policy")])),Pe(" · "),s(Be,j([De("?terms"),Oe("link")]),j([Pe("Terms of use")]))]))]))]))),title:b(n.page,ge)?"Elimination":Ge(n.page)+" | Elimination"}}});ve={Main:{init:Ye(Lt(m))(0)}},n.Elm?function n(r,t,e){for(var u in e)u in t?"init"==u?I(6,r):n(r+"."+u,t[u],e[u]):t[u]=e[u]}("Elm",n.Elm,ve):n.Elm=ve}(this)}).call(n);n.Elm.Main.init()}();
//# sourceMappingURL=bundle.js.map