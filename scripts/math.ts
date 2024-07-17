// Source: https://github.com/jgm/pandoc/issues/6651#issuecomment-1099727774

import { readLines } from "https://deno.land/std@0.224.0/io/mod.ts";
import katex from "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.mjs";

for await (const line of readLines(Deno.stdin)) {
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
