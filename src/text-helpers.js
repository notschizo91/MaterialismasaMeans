// opentype.js glue: font parsing and glyph layout. Geometry (flattening,
// holes, extrusion) all happens on the F# side — this module only produces
// raw glyph path commands in millimetre coordinates (y-down).
import { parse } from 'opentype.js';
import defaultFontUrl from '@fontsource/baloo-2/files/baloo-2-latin-700-normal.woff?url';

export async function loadDefaultFont() {
  const res = await fetch(defaultFontUrl);
  return parse(await res.arrayBuffer());
}

export function parseFontBuffer(buffer) {
  return parse(buffer);
}

export function fontName(font) {
  try {
    return font.names.fullName.en || font.names.fontFamily.en || 'font';
  } catch {
    return 'font';
  }
}

/**
 * Lay out `text` along a baseline at y=0.
 * Returns one command list (opentype path commands, mm units, y-down) per
 * visible glyph.
 */
export function layoutText(font, text, sizeMm, letterSpacingMm, wordGapMul) {
  const scale = sizeMm / font.unitsPerEm;
  let x = 0;
  let prev = null;
  const glyphs = [];
  for (const ch of text) {
    const glyph = font.charToGlyph(ch);
    if (prev) x += font.getKerningValue(prev, glyph) * scale;
    const adv = glyph.advanceWidth * scale;
    if (/\s/.test(ch)) {
      x += adv * wordGapMul + letterSpacingMm;
      prev = null;
      continue;
    }
    glyphs.push(glyph.getPath(x, 0, sizeMm).commands);
    x += adv + letterSpacingMm;
    prev = glyph;
  }
  return glyphs;
}
