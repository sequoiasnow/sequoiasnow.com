# Colors

This is the central colorsheme for the main site. Most of these colors come from [Flat UI Colors](https://flatuicolors.com) but some of them are altered slightly. All colors follow the `rgb` convention, and are ocassionally altered by component's to create lighter or softer hues, usually this is done thie the `rgba` function.



## Specific Colors

Currently there are a variety of colors which are specified for certain things across the website. Some component's notably lack these, in most cases they use `$color--primary` or have accessors for all the colors, this is shown below.

| Variable Name          | Value              | Description                              |
| ---------------------- | ------------------ | ---------------------------------------- |
| `$color--primary`      | `color--orange`    | This color is used for things such as navigation or links, or in other scenarios when it is preferable to have a uniform color across a site This is largelly inspired by github's use of blue sitewide. |
| `$color--nav-selected` | `$color--primary`  | The color of a selected element in the navgiation, currently it relates to the primary color, but that can be changed, by having its own independent variable we reatain that ability. |
| `$color--bg`           | `$color--whiteish` | The color for the background of the site, mostly manifesting as the body component. |
| `$color--content`      | `$color--white`    | The color of any component used on the background, usually manifesting itself as the card component. |



## All Colors

In addition to the individual colors, the Colors Atom also export's all colors themselves. For the styles this is the `$colors--all` hashmap, the names of these colors can also be accessed from javascript as the `allColors` variable which is an export of the Colors Atom. 

To demonstrate the use of these array's we can create a component that alters it's color based on a parameter broght it, the code might look like the following.

```jsx
const ColorContent = ({ children, color }) => {
  return (
  	<div className={`cc cc--${color}`}>{children</div>
  )
}
```

Now, while this might seem complex to implement at first glance, due to the `$colors-all` variable we can account for any passed color in css like the following.

```scss
@each $name, $color in $colors-all {
  .cc--#{$name} { color: $color; }
}
```



