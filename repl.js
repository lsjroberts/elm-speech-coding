/*
module Main exposing (..)

main : Int -> Int
main x =
  x + 1

update : Msg -> Model -> Model
update msg model =
  case msg of
    Foo -> model + 1
    Bar -> model + 2

view : Model -> Html
view model =
  let
    foo = model + 1
  in
    foo + 2
*/

const ast = {
  type: 'module',
  start: 0,
  end: 61,
  loc: {
    start: {
      line: 1,
      column: 0
    },
    end: {
      line: 5,
      column: 7
    }
  },
  body: [
    {
      type: 'ModuleDeclaration',
      loc: {},
      id: {
        type: 'Identifier',
        name: 'Main'
      },
      exposingAll: true
    },
    {
      type: 'FunctionTypeDeclaration',
      loc: {},
      id: {
        type: 'Identifier',
        name: 'main'
      },
      body: [
        {
          type: 'TypeLiteral',
          name: 'Int'
        },
        {
          type: 'TypeLiteral',
          name: 'Int'
        }
      ]
    },
    {
      type: 'FunctionDeclaration',
      loc: {},
      id: {
        type: 'Identifier',
        name: 'main'
      },
      params: [
        {
          type: 'Identifier',
          name: 'x'
        }
      ],
      body: {
        type: 'BinaryExpression',
        loc: {},
        operator: '+',
        left: {
          type: 'Identifier',
          name: 'x'
        },
        right: {
          type: 'IntLiteral',
          value: 1
        }
      }
    },
    {
      type: 'FunctionTypeDeclaration',
      loc: {},
      id: {
        type: 'Identifier',
        name: 'update'
      },
      body: [
        {
          type: 'TypeAlias',
          name: 'Msg'
        },
        {
          type: 'TypeAlias',
          name: 'Model'
        },
        {
          type: 'TypeAlias',
          name: 'Model'
        }
      ]
    },
    {
      type: 'FunctionDeclaration',
      loc: {},
      id: {
        type: 'Identifier',
        name: 'update'
      },
      params: [
        {
          type: 'Identifier',
          name: 'msg'
        },
        {
          type: 'Identifier',
          name: 'model'
        }
      ],
      body: {
        type: 'CaseStatement',
        of: {
          type: 'Identifier',
          name: 'msg'
        },
        body: [
          {
            type: 'CasePattern',
            match: {
              type: 'TypeValue',
              value: 'Foo'
            },
            body: {
              type: 'BinaryExpression',
              loc: {},
              operator: '+',
              left: {
                type: 'TypeAlias',
                name: 'model'
              },
              right: {
                type: 'IntLiteral',
                value: 1
              }
            }
          },
          {
            type: 'CasePattern',
            match: {
              type: 'TypeValue',
              value: 'Bar'
            },
            body: {
              type: 'BinaryExpression',
              loc: {},
              operator: '+',
              left: {
                type: 'TypeAlias',
                name: 'model'
              },
              right: {
                type: 'IntLiteral',
                value: 2
              }
            }
          }
        ]
      }
    },
    {
      type: 'FunctionDeclaration',
      loc: {},
      id: {
        type: 'Identifier',
        name: 'view'
      },
      params: [
        {
          type: 'Identifier',
          name: 'model'
        }
      ],
      body: {
        type: 'LetInStatement',
        let: [
          {
            type: 'FunctionDeclaration',
            loc: {},
            id: {
              type: 'Identifier',
              name: 'foo'
            },
            params: [],
            body: {
              type: 'BinaryExpression',
              operator: '+',
              left: {
                type: 'Identifier',
                name: 'model'
              },
              right: {
                type: 'IntLiteral',
                value: 1
              }
            }
          }
        ],
        in: {
          type: 'BinaryExpression',
          operator: '+',
          left: {
            type: 'Identifier',
            name: 'foo'
          },
          right: {
            type: 'IntLiteral',
            value: 2
          }
        }
      }
    }
  ]
};

const render = node =>
  renderers[node.type]
    ? renderers[node.type](node)
    : `ERROR (${JSON.stringify(node)})`;
const renderers = {
  module: node => node.body.map(render).join('\n'),
  ModuleDeclaration: node => {
    const id = `module ${render(node.id)}`;
    const exposing = node.exposingAll ? ' exposing (..)' : '';
    return `${id}${exposing}\n`;
  },
  FunctionTypeDeclaration: node =>
    `${render(node.id)} : ${node.body.map(render).join(' -> ')}`,
  FunctionDeclaration: node =>
    `${render(node.id)} ${node.params.map(render).join(' ')} =\n  ${render(
      node.body
    )}\n`,
  BinaryExpression: node =>
    `${render(node.left)} ${node.operator} ${render(node.right)}`,
  Identifier: node => `${node.name}`,
  IntLiteral: node => `${node.value}`,
  TypeLiteral: node => node.name,
  TypeAlias: node => node.name,
  TypeValue: node => node.value,
  CaseStatement: node =>
    `case ${render(node.of)} of\n    ${node.body.map(render).join('\n    ')}`,
  CasePattern: node => `${render(node.match)} ->\n      ${render(node.body)}`,
  LetInStatement: node =>
    `let\n    ${node.let.map(render).join('\n    ')}\n  in\n    ${render(
      node.in
    )}`
};

let activeNodeIndex = [0];

const getActiveNode = () => {
  let node;
  activeNodeIndex.forEach(i => {
    if (!node) {
      node = ast.body[i];
      return;
    }

    switch (node.type) {
      case 'FunctionDeclaration':
        if (i === 0) node = node.id;
        if (i === 1) node = node.params;
        if (i === 2) node = node.body;
        break;

      default:
        if (Array.isArray(node)) {
          node = node[i];
        }
        break;
    }
  });
  return node;
};

const next = () => {
  if (!activeNodeIndex) return;
  const activeNode = getActiveNode();
  activeNodeIndex = nexters[activeNode.type]
    ? nexters[activeNode.type](activeNodeIndex, activeNode)
    : activeNodeIndex;
  console.log('next', activeNodeIndex);
};
const nexters = {
  FunctionDeclaration: (nodeIndex, node) => {
    console.log('nexters', 'FunctionDeclarartion', nodeIndex);
    return [nodeIndex[0], 0];
  },
  BinaryExpression: (nodeIndex, node) => node.left,
  Identifier: (nodeIndex, node) => {
    if (nodeIndex.length > 1) {
      return [nodeIndex[0], nodeIndex[1] + 1];
    }
  }
  // Identifier: node => console.error('todo: find the parent and go from there')
};

const addNode = node => {
  const newIndex = ast.body.push(node) - 1;
  activeNodeIndex = [newIndex];
};

const goToPriority = {
  FunctionDeclaration: 1,
  FunctionTypeDeclaration: 2,
  ModuleDeclaration: 3
};

const goTo = id => {
  let matches = ast.body.filter(
    declaration => declaration.type === 'Identifier' && declaration.name === id
  );

  // Check again, one level deeper, but no further
  if (matches.length === 0) {
    matches = ast.body.filter(
      declaration => declaration.id && declaration.id.name === id
    );
  }

  if (matches.length === 0) {
    console.error('error: could not find identifer', id);
  } else if (matches.length === 1) {
    location = matches[0];
  } else if (matches.length > 1) {
    console.info('info: multiple identifiers called', id, 'found');
    matches.sort((a, b) => goToPriority[a.type] - goToPriority[b.type]);
    location = matches[0];
  }
};

const newFunction = name => {
  addNode({
    type: 'FunctionDeclaration',
    loc: {},
    id: {
      type: 'Identifier',
      name: ''
    },
    params: [],
    body: {}
  });
  next();
};
const addTypeSignature = () => {};
const setActiveNodeNameOrValue = nameOrValue => {
  let activeNode = getActiveNode();
  console.log('setActiveNodeNameOrValue', activeNode, nameOrValue);

  if (!activeNode) {
    throw new Error(
      `Can not set name or value as there is no active node: ${nameOrValue}`
    );
  }

  switch (activeNode.type) {
    case 'Identifier':
      activeNode.name = nameOrValue;
      break;

    case 'IntLiteral':
      activeNode.value = nameOrValue;
      break;

    default:
      if (typeof nameOrValue === 'number') {
        activeNode.type = 'IntLiteral';
        activeNode.value = nameOrValue;
      } else if (typeof nameOrValue === 'string') {
        activeNode.type = 'Identifier';
        activeNode.name = nameOrValue;
      }
  }
  next();
};

let constructingCommand;

const command = (cmd, ...params) => {
  console.log('command', cmd);
  switch (cmd) {
    case 'go to':
      goTo(...params);
      break;

    case 'new function':
      newFunction();
      break;

    case 'type signature':
      addTypeSignature();
      break;

    default:
      // set the name/value of the active node
      setActiveNodeNameOrValue(cmd);
      break;
  }
};

// command('go to', 'update');
command('new function');
command('init');
command('one');
// command('type signature');
// command('model');

console.log('FINISH', { activeNodeIndex, activeNode: getActiveNode() });
console.log(render(ast));
