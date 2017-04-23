# Card

The card is a simple base unit upon which to build other things, indeed all component's on this page are placed in cards, or are using the card mixin. 



## The Card Component

The card component is very simple, it simply wrapps the content with an appropriate color and background. For instance, a button wrapped in a card is simple `<Card><Button></Card>`. There is no great complexity. The card can take one argument, `dark: boolean`. If dark is set to true, then the card will change the color of text and become much darker. An example of this is shown below.

## The Card Mixin

Along with a component the card also provides two mixins. `card()` and `card-dark()` which respectivly give all styles of the card, and all the styles of the card with a dark background. 

The Card Atom also exposes two variables in styles

| Variable       | Value  | Description                              |
| -------------- | ------ | ---------------------------------------- |
| `card--margin` | `10px` | The margin a card uses to place distance between itself and other cards. This is actually acomplished with a trick of padding so that the card itself doesn't effect the padding or margin of its contained element's |
| `card—padding` | `20px` | The padding applied to the content.      |

