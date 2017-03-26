import React from 'react';
import { Match
       , Miss
       , hashHistory
       , Router
       , Route } from 'react-router'
import { connect } from 'react-redux'
import Header from './Header'
import BlogPost from './BlogPost'
import MainPage from './MainPage'
import Mars from './Mars';
import Hello from './Hello';

const Routes = () => {
  return (
    <div>
      <Header />
      <Router history={hashHistory}>
        <Route path="/" component={MainPage} />
        <Route path="/blog/example" component={BlogPost} />
      </Router>
    </div>
  ) 
}

export default Routes
