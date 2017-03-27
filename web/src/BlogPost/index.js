import React from 'react';

const BlogPost = ({ body = 'Example Body', title = 'Test 1', url = '' }) => {
  return (
    <article>
      <header>
        <a class="title" href={url}>{title}</a>
      </header>
      <body>
        {body}
      </body>
    </article>
  )
}

export default BlogPost
