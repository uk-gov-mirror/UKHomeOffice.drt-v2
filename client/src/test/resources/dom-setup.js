const jsdom = require("jsdom");
const { JSDOM } = jsdom;
const { ResizeObserver } = require("@juggle/resize-observer");

const dom = new JSDOM("<!doctype html><html><body></body></html>", { url: "http://localhost/" });

global.window = dom.window;
global.document = dom.window.document;
global.navigator = dom.window.navigator;
global.HTMLElement = dom.window.HTMLElement;
global.Node = dom.window.Node;
global.getComputedStyle = dom.window.getComputedStyle;
global.MutationObserver = dom.window.MutationObserver;
global.ResizeObserver = ResizeObserver;
global.requestAnimationFrame = (cb) => setTimeout(cb, 0);
global.cancelAnimationFrame = (id) => clearTimeout(id);

console.log("[dom-setup] document ready:", !!global.document);
