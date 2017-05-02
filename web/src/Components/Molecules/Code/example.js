import React from 'react'

/* --- Atoms --- */
import Card from '../../Atoms/Card'

/* --- Molecules --- */
import Code from './index'

const Example = () => (
  <div>
    <Card>
      Example flush card. <Code>This</Code> is what inline code looks
      like.
      <Code language="c" flush>
        {`
#include <stdio.h>

int main() {
    printf("Hello World");
}
        `}
      </Code>
    </Card>
  </div>
)
export default Example
