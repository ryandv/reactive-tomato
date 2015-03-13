require.config({
  baseUrl: '/js'
});

require(
  [
    'react',
    'JSXTransformer',
    'jquery-1.11.2',
    'underscore',
    'reactivetomato'
  ],

  function(React, jsxtrans, $, _, ReactiveTomato) {
    React.render(
      React.createElement(ReactiveTomato.ReactiveTomato, {}),
      document.getElementById('elmodoro-wrapper')
    );
});
