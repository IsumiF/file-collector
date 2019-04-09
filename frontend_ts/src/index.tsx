import * as React from 'react';
import * as ReactDOM from 'react-dom';
import Button from '@material-ui/core/Button';

function App() {
  return (
    <Button variant="contained" color="primary">
      Hello, world.
    </Button>
  );
}

ReactDOM.render(
  <App /> ,
  document.querySelector('#app_root')
);