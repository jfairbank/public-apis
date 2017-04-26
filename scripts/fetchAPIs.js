const fs = require('fs');
const path = require('path');
const fetch = require('node-fetch');

const promisify = fn => (...args) => new Promise((resolve, reject) => {
  fn(...args, (err, result) => {
    if (err) {
      reject(err);
    } else {
      resolve(result);
    }
  });
});

const readFile = promisify(fs.readFile);
const writeFile = promisify(fs.writeFile);

function parseEntry(categoryName, line) {
  const linkRegex = /\((.+)\)/;
  const [, api, description, auth, https, link] = line.split('|');

  return {
    name: api.trim(),
    category: categoryName,
    description: description.trim(),
    auth: auth.trim().replace(/`/g, ''),
    https: https.trim(),
    link: (link.match(linkRegex) || ['', ''])[1],
  };
}

function parse(contents) {
  const READY = 'READY';
  const HEADER = 'HEADER';
  const APIS = 'APIS';

  const categoryRegex = /^### (.+)/;
  const headerStopRegex = /^\|---\|/;

  const lines = contents.split('\n');

  return lines.reduce(({ state, entries, currentCategoryName }, line) => {
    switch (state) {
      case READY: {
        let match;

        if (match = line.match(categoryRegex)) {
          const category = match[1].trim();

          state = HEADER;
          currentCategoryName = match[1].trim();
        }

        break;
      }

      case HEADER: {
        if (headerStopRegex.test(line)) {
          state = APIS;
        }

        break;
      }

      case APIS: {
        if (!line.trim()) {
          state = READY;
          currentCategoryName = '';
        } else {
          const entry = parseEntry(currentCategoryName, line);
          entries.push(entry);
        }

        break;
      }
    }

    return { state, entries, currentCategoryName };
  }, {
    state: READY,
    entries: [],
    currentCategoryName: '',
  }).entries;
}

async function fetchSource() {
  const url = 'https://raw.githubusercontent.com/toddmotto/public-apis/master/README.md';

  const response = await fetch(url);

  if (!response.ok) {
    const error = new Error(response.statusText);
    error.response = response;
    throw error;
  }

  return response.text();
}

async function main() {
  try {
    const data = parse(await fetchSource());
    const json = JSON.stringify(data, null, 2);
    const contents = `module.exports = ${json};`;
    const file = path.resolve(__dirname, '../src/apis.js');

    await writeFile(file, contents);

    console.log('Updated APIs successfully.');
  } catch (e) {
    console.error('\nError updating APIs');
    console.error('-------------------');
    console.error(e);
  }
}

main();
