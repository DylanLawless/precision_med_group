!function(e){var n={};function t(o){if(n[o])return n[o].exports;var a=n[o]={i:o,l:!1,exports:{}};e[o].call(a.exports,a,a.exports,t);a.l=!0;return a.exports}var o=[{name:"head-dlb/bundle.production.js",path:"head-dlb/static-1.292/bundle.production.js",ids:{}}];t.dlbpr=function(e,n){var a=o[e];if(!a.r){a.r=window["__webpack_require_"+a.name+"__"];if(!a.r)throw new Error("dlb "+a.name+" not loaded");a.r.linkDlb(t,a.ids)}return a.r(n)};t.m=e;t.c=n;t.d=function(e,n,o){t.o(e,n)||Object.defineProperty(e,n,{enumerable:!0,get:o})};t.r=function(e){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"});Object.defineProperty(e,"__esModule",{value:!0})};t.t=function(e,n){1&n&&(e=t(e));if(8&n)return e;if(4&n&&"object"==typeof e&&e&&e.__esModule)return e;var o=Object.create(null);t.r(o);Object.defineProperty(o,"default",{enumerable:!0,value:e});if(2&n&&"string"!=typeof e)for(var a in e)t.d(o,a,function(n){return e[n]}.bind(null,a));return o};t.n=function(e){var n=e&&e.__esModule?function(){return e.default}:function(){return e};t.d(n,"a",n);return n};t.o=function(e,n){return Object.prototype.hasOwnProperty.call(e,n)};t.p="//static.hsappstatic.net/adsscriptloaderstatic/static-1.346/";t(t.s=0)}([function(e,n,t){"use strict";t.r(n);const o="data-hsjs-portal",a="data-hsjs-env",i="data-hsjs-hublet",r={PROD:"prod",QA:"qa"};function d(e){if(!e)return null;const n=document.querySelectorAll(`script[${e}]`);return n.length?n[0].getAttribute(e):null}function s(){return d(a)||r.PROD}function c(){let e=d(o);e=parseInt(e,10);if(!e)throw new Error("HS Pixel Loader can't identify portalId via "+o);return e}function l(){return d(i)||"na1"}function u(){return"withCredentials"in new XMLHttpRequest}function p(e,n){!function(e,n,t,o,a,i,r){if(!e.fbq){a=e.fbq=function(){a.callMethod?a.callMethod.apply(a,arguments):a.queue.push(arguments)};e._fbq||(e._fbq=a);a.push=a;a.loaded=!0;a.version="2.0";a.agent="tmhubspot";a.queue=[];(i=n.createElement(t)).async=!0;i.src=o;(r=n.getElementsByTagName(t)[0]).parentNode.insertBefore(i,r)}}(window,document,"script","https://connect.facebook.net/en_US/fbevents.js");for(var t=0;t<e.length;t++){e[t].limitedDataUseEnabled&&fbq("dataProcessingOptions",["LDU"],0,0);fbq("init",e[t].pixelId,{external_id:n})}fbq("track","PageView")}function f(e){const n=document.createElement("script");n.async=!0;n.src="https://www.googletagmanager.com/gtag/js?id=AW-"+e;document.head.appendChild(n)}function w(e){window.dataLayer=window.dataLayer||[];var n="qa"===s()?"dZWU5Zm":"dZTQ1Zm";function t(){dataLayer.push(arguments)}t("js",new Date);t("set","developer_id."+n,!0);for(var o=0;o<e.length;o++)t("config","AW-"+e[o].pixelId)}function h(e){for(var n=0;n<e.length;n++){const t=e[n].pixelId;window._linkedin_data_partner_ids=window._linkedin_data_partner_ids||[];window._linkedin_data_partner_ids.push(t)}!function(){var e=document.getElementsByTagName("script")[0],n=document.createElement("script");n.type="text/javascript";n.async=!0;n.src="https://snap.licdn.com/li.lms-analytics/insight.min.js";e.parentNode.insertBefore(n,e)}()}function b(e,n){for(var t in e)if(e.hasOwnProperty(t)&&e[t].length>0){var o=e[t];switch(t){case"FACEBOOK":if(n&&!e.loadedFbPixel){p(o,n);e.loadedFbPixel=!0}break;case"ADWORDS":f(o[0].pixelId);w(o);break;case"LINKEDIN":h(o)}}}function g(e,n){for(var t in e)if(e.hasOwnProperty(t)&&e[t].length>0)switch(t){case"FACEBOOK":if(!e.loadedFbPixel){p(e[t],n);e.loadedFbPixel=!0}}}function m(e,n){for(var t in e)if(e.hasOwnProperty(t)&&e[t].length>0)switch(t){case"FACEBOOK":fbq("consent","grant");break;case"ADWORDS":dataLayer.push("consent","update",{ad_storage:"granted",analytics_storage:"granted"})}}function v(e){if(e.hasOwnProperty("LINKEDIN"))window.location.reload();else for(var n in e)if(e.hasOwnProperty(n)&&e[n].length>0)switch(n){case"FACEBOOK":fbq("consent","revoke");break;case"ADWORDS":dataLayer.push("consent","update",{ad_storage:"denied",analytics_storage:"denied"})}}function _({jsonUrl:e,jsonpUrl:n},t,o){if(!e&&!n)throw new Error("Missing jsonUrl and jsonpUrl args");u()?O(e,t):j(n,t,o)}const y=function(e){return`https://${e}?portalId=${c()}`},O=function(e,n){const t=new XMLHttpRequest;t.addEventListener("load",()=>{const e=JSON.parse(t.responseText);n(e)});t.open("GET",y(e));t.send()},E=e=>"hubspotJsonpCallbackName"+e,P=function(e,n){return`https://${e}?${["portalId="+c(),"callback="+n].join("&")}`},j=function(e,n,t){const o=document.createElement("script"),a=E(t);window[a]=function(e){n(e);document.body.removeChild(o);delete window[a]};o.src=P(e,a);document.body.appendChild(o)},S=function(){const e="qa"===s()?"qa":"",n=l(),t=`api${"na1"!==n&&n?"-"+n:""}.hubapi${e}.com`;let o=null,a=null;window.enabledEventSettings={};if(!(window.disabledHsPopups&&window.disabledHsPopups.indexOf("ADS")>-1)){window._hsp=window._hsp||[];window._hsp.push(["addPrivacyConsentListener",function(e){e.categories.advertisement?o?m(o,a):_({jsonUrl:t+"/hs-script-loader-public/v1/config/pixels-and-events/json",jsonpUrl:t+"/hs-script-loader-public/v1/config/pixels-and-events/jsonp"},e=>{o=e.pixels;b(e.pixels,a);window.enabledEventSettings={data:e.enhancedConversionEventSettings,portalId:c()}},"addPixels"):o&&v(o)}]);window._hsq=window._hsq||[];window._hsq.push(["addUserTokenListener",function(e){a=e;o&&g(o,a)}]);window.addEventListener("message",e=>{let n;if("hsFormCallback"===e.data.type&&"onFormSubmitted"===e.data.eventName){n=function(){window.dataLayer=window.dataLayer||[];window.dataLayer.push(arguments)};const{data:{data:{conversionId:o,formGuid:a}}}=e,{enabledEventSettings:{data:i,portalId:r}}=window;i.forEach(({hubSpotFormId:e,pixelId:i,conversionLabel:d})=>{if(a===e&&null!==d){n("event","conversion",{send_to:`AW-${i}/${d}`,transaction_id:o});fetch(`https://${t}/hs-script-loader-public/v1/config/gtag-conversion-monitor`,{method:"POST",headers:{"Content-Type":"application/json"},body:JSON.stringify({portalId:r,hubSpotFormId:e,formSubmissionId:o,pixelId:i,conversionLabel:d,webpageUrl:window.location.href})})}})}},!1)}};window.PIXELS_RAN=window.PIXELS_RAN||!1;if(!window.PIXELS_RAN){window.PIXELS_RAN=!0;S()}}]);
//# sourceMappingURL=pixels-release.js.map