globalThis.window = {
  devicePixelRatio: 1,
  addEventListener: () => {},
  requestAnimationFrame: () => {}
};
globalThis.document = {
  getElementById: () => null,
  addEventListener: () => {}
};
globalThis.localStorage = {
  getItem: () => null,
  setItem: () => {}
};

const { bench } = await import('../public/sapper/solver.mjs');

bench();
