import { JSDOM } from "jsdom";
import { ResizeObserver as RO } from "@juggle/resize-observer";

const dom = new JSDOM("<!doctype html><html><body></body></html>", { url: "http://localhost/" });

globalThis.window = dom.window;
globalThis.document = dom.window.document;
globalThis.navigator = dom.window.navigator;

globalThis.HTMLElement = dom.window.HTMLElement;
globalThis.Node = dom.window.Node;
globalThis.getComputedStyle = dom.window.getComputedStyle;
globalThis.MutationObserver = dom.window.MutationObserver;

if (!globalThis.ResizeObserver) globalThis.ResizeObserver = RO;
if (!globalThis.requestAnimationFrame) globalThis.requestAnimationFrame = (cb) => setTimeout(cb, 0);
if (!globalThis.cancelAnimationFrame) globalThis.cancelAnimationFrame = (id) => clearTimeout(id);

console.log("[dom-setup] ESM bootstrap ready:", !!globalThis.document);
