import React from 'react'
import Page from '../Page'
import SyntaxHighlight from '../SyntaxHighlight'
import moment from 'moment'

const Home = ({ refreshAllPosts, postData }) => {
  return (
    <Page title="Sequioa's Blog">
      <SyntaxHighlight language="json" code={JSON.stringify(postData, null, '   ')} />
    </Page>
  )
}

import { connect } from 'react-redux'
import { refreshAllExcerpts } from '../actions'

const ConnectedHome = connect(
  (state) => {
    return { postData : state.posts.excerpts } 
  }
)(Home)


export default ConnectedHome
