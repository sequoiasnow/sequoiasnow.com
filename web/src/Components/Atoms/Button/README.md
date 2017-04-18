# Button


The button component is very simple, it is a small clickable component to be used throught the site. It handles one simple onClick event's but has no internal state. The button component also accepts several modification styles as described in the properties. One known difficulty with the button is alignment.

## Properties

There are a variety of argument's that the button can take to modify its colors. Note `$variable` indicates there are a variety of options, for `$color` these include all [colors](/atoms/Color) or for sizes all [sizes](/atoms/sizes).

### CSS Modifiers

| Name     | Class            | Description                      |
| :------- | :--------------- | :------------------------------- |
| color    | `.btn--$color`   | Changes the color of the button. |
| size     | `.btn--$size`    | Changes the size(padding).       |
| inactive | `.btn--inactive` | Makes the button inactive        |



### Functional Modifiers

Unlike the previous, these modifiers update the usage and behavior of the actual componet

| Name    | Type       | Description                              |
| ------- | ---------- | ---------------------------------------- |
| onClick | `function` | A function that is triggered on a click event on the button. The funciton is passed the `event` variable from the click that has occured. |

## Usage

A simple example would be to expand a view when a button is clicked. This is easily accomplished with the button component in a simple example as follows. This uses states and assumes the `Button` component has been imported.



```jsx
class ExpandView extends React.Component {
  constructor(props) {
    super(props)

    // Set initial state
    this.state = { expanded: false }
  }

  render() {
    const { expanded } = this.state
    return (
      <div>
      	<Button onClick={() => this.setState({ expanded: !expanded })}>
          {expanded ? 'Expand' : 'Collapse'}
        </Button>
      	{expanded && <HiddenView />}
      </div>
    )
  }
}
```

