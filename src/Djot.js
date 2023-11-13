import * as djot from "@djot/djot";

export function renderHtml_(ast) {
  return function (options) {
    console.log("options", options);
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

export function buildOverridesImpl(isJust, overrides) {
  return Object.fromEntries(
    Object.entries(overrides)
      .map(([k, v]) => {
        return isJust(v)
          ? [k, (node, renderer) => v.value0(node)(renderer)]
          : null;
      })
      .filter((i) => i !== null)
  );
}

export function renderHtmlImpl(isJust, renderer, opts) {
  let options = undefined;
  if (isJust(opts)) {
    const inner = opts.value0.value0;
    const overrides = isJust(inner.overrides) ? inner.overrides.value0 : {};
    const warn = isJust(inner.warn) ? inner.warn.value0 : null;
    options = { overrides, warn };
  }
  console.log("options", options);
  return djot.renderHTML(renderer, options);
}

// export const renderChildren_ = (node) => (renderer) => {
//   return renderer.renderChildren(node);
// };
