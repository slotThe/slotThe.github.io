// Source: https://github.com/jgm/pandoc/issues/6651#issuecomment-1099727774

import { createInterface } from "node:readline"

// I don't want to create a package.json…
import { createRequire } from "module";
const katex = createRequire(import.meta.url)('katex');

for await (const line of createInterface({ input: process.stdin })) {
  try {
    let DISPLAY    = ":DISPLAY ";
    let useDisplay = line.startsWith(DISPLAY);
    let cleanLine  = useDisplay ? line.substring(DISPLAY.length) : line;
    console.log(katex.renderToString(cleanLine, {
      displayMode: useDisplay,
      strict: "error",
      throwOnError: true,
    }));
  } catch (error) {
    throw new Error(`Input: ${line}\n\nError: ${error}`);
  }
}
