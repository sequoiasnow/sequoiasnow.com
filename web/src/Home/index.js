import React from 'react'
import Page from '../Page'
import SyntaxHighlight from '../SyntaxHighlight'

const Home = ({ refreshAllPosts, postData }) => {
  refreshAllPosts()
  return (
    <Page title="Sequoia's Blog">
      <SyntaxHighlight language="json" code={JSON.stringify(postData)} />
    </Page>
  )
}

import { connect } from 'react-redux'
import { refreshAllPosts } from '../actions'

const ConnectedHome = connect(
  (state) => {
    return { postData : state.posts.all }
  },
  (dispatch) => {
    return { refreshAllPosts: () => { dispatch(refreshAllPosts()) } }
  }
)(Home)


export default ConnectedHome
