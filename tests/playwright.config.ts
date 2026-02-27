import { defineConfig } from '@playwright/test';
import * as path from 'path';

const projectRoot = path.resolve(__dirname, '..');

export default defineConfig({
  testDir: './e2e',
  timeout: 30000,
  retries: 1,
  use: {
    baseURL: 'http://127.0.0.1:4021',
    headless: true,
  },
  webServer: {
    command: `cargo run -- ${path.join(projectRoot, 'example')} --port 4021`,
    cwd: projectRoot,
    url: 'http://127.0.0.1:4021/health',
    reuseExistingServer: !process.env.CI,
    timeout: 120000,
  },
});
