import * as djot from "@djot/djot";

export function renderHtml_(ast) {
  return function (options) {
    return djot.renderHTML(ast, options);
  };
}

export function parse_(input) {
  return function (options) {
    const res = djot.parse(input, options);
    return res;
  };
}

export function renderWarning_(warn) {
  return warn.render();
}
