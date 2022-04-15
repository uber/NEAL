import commonjs from '@rollup/plugin-commonjs';
import resolve from '@rollup/plugin-node-resolve';
import shebang from 'rollup-plugin-preserve-shebang';
import { terser } from 'rollup-plugin-terser';

export default {
  input: 'src/index.js',
  external: ['fs'],
  output: {
    file: '_build/dump_javascript_ast.js',
    format: 'cjs',
    exports: 'default',
  },
  plugins: [
    shebang(),
    resolve(),
    commonjs(),
    terser(),
  ]
};
