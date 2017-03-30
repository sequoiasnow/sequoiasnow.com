import React from 'react';
import moment from 'moment'
import { connect } from 'react-redux'
import { stringToSymbols
       , ElementSVG
       , RawElement
       , elementColors } from '../Element';
import { times } from '../utilities'
import Kute from 'kute.js'
import 'kute.js/kute-svg'
import 'kute.js/kute-attr'

const mapStateToProps = ({ browser }) => {
  return {
    browser
  }
}

@connect(mapStateToProps)
export default class NavSVG extends React.Component {
  constructor(props) {
    super(props)

    /* Bind local functions. */
    this.mouseOver = this.mouseOver.bind(this)
    this.mouseOut  = this.mouseOut.bind(this)
  }

  componentDidMount() {
    /* Animate the stripes */
    Kute.fromTo(
      this.stripeOne,
      { svgTransform: { translate: [-200, 200] } },
      { svgTransform: { translate: [0, 0] } },
      { delay: 0 }
    ).start()
    Kute.fromTo(
      this.stripeTwo,
      { svgTransform: { translate: [-300, 300] } },
      { svgTransform: { translate: [0, 0] } },
      { delay: 500 }
    ).start()
    Kute.fromTo(
      this.stripeThree,
      { svgTransform: { translate: [-400, 400] } },
      { svgTransform: { translate: [0, 0] } },
      { delay: 1000 }
    ).start()
    Kute.fromTo(
      this.stripeFour,
      { svgTransform: { translate: [-500, 500] } },
      { svgTransform: { translate: [0, 0] } },
      { delay: 1500 }
    ).start()

    /**/
    
    
  }

  mouseOver() {
    Kute.fromTo(
      this.navHomeBg, 
      { path: 'M375 125 L300 125 L300 200 Z' }, {
        path: 'M375 125 L240 40 L240 260 Z',
        morphIndex: 126
      }).start()
    Kute.to(this.symbolH, { svgTransform: {
      translate: [-61, -33]
    }}).start()
    Kute.to(this.symbolO, { svgTransform: {
      translate: [-61, 33]
    }}).start()
    Kute.to(this.symbolMe, { svgTransform: {
      translate: [0, 0]
    }}).start()
    Kute.fromTo( this.navHomeSelectedBg, { draw: '0% 0%' }, { draw: '0% 100%' } ).start()
    Kute.to(this.navHomeSelectedBg, { attr: { fillOpacity: 1 } }).start()
  }

  mouseOut() {
    Kute.fromTo(
      this.navHomeBg, 
      { path: 'M375 125 L240 40 L240 260 Z' }, {
        path: 'M375 125 L300 125 L300 200 Z',
        morphIndex: 126
      }).start()
    Kute.to(this.symbolO, { svgTransform: {
      translate: [61, 0]
    }}).start()
    Kute.to(this.symbolH, { svgTransform: {
      translate: [0, 0]
    }}).start()
    Kute.to(this.symbolMe, { svgTransform: {
      translate: [122, 0]
    }}).start()
    Kute.to(this.navHomeSelectedBg, { attr: { fillOpacity: 0 } }).start()
    Kute.fromTo(this.navHomeSelectedBg, { draw: '0% 100%' }, { draw: '-10% 0%' } ).start()
  }

  
  
  render() {
    const { onHomeClick, title, browser } = this.props
    const weekday = moment().format('dddd')
    const colors = Object.values(elementColors)
    const randomColor = colors[Math.floor(Math.random() * colors.length)]
    const animationDelay = 3000
    const nSideStripes = 8
    
    return (
      <svg viewBox="0 0 1000 300" xmlns="http://www.w3.org/2000/svg"> 
        <defs> 
        </defs>
        <g id="Page-1" stroke="none" strokeWidth="1" fill="none" fillRule="evenodd">
          <g id="navigation">
            <polygon id="nav-background" fill="#FEEDB3" points="0 500 0 0 500 0"></polygon> 
            <path d="M177.071826,122.928174 L0,300 L0,400 L400,0 L300,0 L202.739874,97.2601257 C205.29471,100.426635 205.796311,104.553549 204.244696,109.640967 C202.535616,115.244678 199.816666,119.910914 196.087764,123.639816 C192.690319,127.03726 189.308463,129.041515 185.942093,129.652641 C182.575723,130.263767 180.136437,129.813198 178.62416,128.300921 C177.878379,127.55514 177.412273,126.560781 177.225828,125.317814 C177.136168,124.720079 177.084834,123.923535 177.071826,122.928174 Z" id="nav-stripe-3" fillOpacity="0.8" fill="#EF634D" ref={(r) => this.stripeThree = r} transform="translate(-400, 400)"></path>
            <path d="M202.739874,97.2601257 C202.480598,96.9387744 202.200176,96.6273143 201.898607,96.3257454 C198.687608,93.1147464 195.031267,92.5864932 190.929475,94.7409699 C188.422824,96.0460856 184.507536,99.3606155 179.183492,104.684659 C178.023389,105.844762 176.863304,106.797689 175.703201,107.54347 C173.94233,108.682856 172.730456,108.921088 172.06754,108.258172 C171.42534,107.615972 171.088708,106.295339 171.057634,104.296234 C171.026559,102.297128 171.311402,100.997211 171.912169,100.396443 C173.445163,99.2363402 174.470595,98.3973498 174.988498,97.8794468 C178.303078,94.5648672 179.949985,90.9396001 179.929269,87.0035368 C179.908553,83.0674735 178.717393,79.9186701 176.355756,77.5570322 C175.278517,76.4797938 173.963063,75.5165085 172.409354,74.6671475 C171.559993,74.2321089 170.513844,73.7660032 169.270877,73.2688162 L174.646684,67.8930093 L172.129688,65.376013 L171.756799,65.7489013 C171.363193,66.1425077 170.741719,66.7950557 169.892358,67.7065651 C169.042997,68.6180745 168.400806,69.2913384 167.965768,69.726377 C166.287762,71.4043829 164.179928,72.7975213 161.642203,73.9058338 C159.104478,75.0141464 157.079507,76.3244215 155.56723,77.8366984 C152.04549,81.3584393 150.40894,85.1701486 150.657534,89.2719409 C150.906127,93.3737332 152.242297,96.6364735 154.666084,99.0602599 C156.654831,101.049008 158.969823,102.317851 161.611129,102.866828 C164.252435,103.415806 166.826374,103.058458 169.333025,101.794774 C169.063715,106.207308 169.032642,109.231817 169.239803,110.868391 C169.446964,112.504964 169.933786,113.706481 170.700282,114.472978 C171.528927,115.301623 172.678655,115.622718 174.149499,115.436273 C174.978144,115.311976 176.055367,114.959807 177.381198,114.379756 L177.381198,115.125533 L177.16368,119.009786 C177.088403,120.547591 177.057785,121.853717 177.071826,122.928174 L0,300 L0,200 L200,0 L300,0 L202.739874,97.2601257 Z M77.4781969,212.231872 L78.627936,213.381611 L99.385387,192.62416 C102.927844,189.081703 105.424099,185.611804 106.874228,182.21436 C109.608756,175.75093 108.490102,170.033366 103.518233,165.061496 C100.555827,162.099091 97.2154026,160.830247 93.4968586,161.254928 C89.7783146,161.679608 86.106437,163.000241 82.4811155,165.216866 C84.262702,162.275177 85.2984926,159.768564 85.5885183,157.696952 C86.1271375,154.050914 84.8945467,150.726026 81.890709,147.722188 C77.8924973,143.723977 73.5784295,142.403344 68.9483761,143.76025 C64.3183227,145.117156 59.4552859,148.343644 54.3591197,153.43981 L36.5226274,171.276302 L37.7034405,172.457115 C40.3965364,170.012613 42.4577597,168.748948 43.8871721,168.666084 C45.3165846,168.583219 47.2017235,169.712231 49.5426453,172.053153 L77.8510853,200.361593 C80.2334394,202.743947 81.372809,204.639444 81.2692284,206.04814 C81.1656478,207.456836 79.9019833,209.518059 77.4781969,212.231872 Z M120.422504,169.846897 L121.292577,170.71697 L136.332407,155.67714 L135.462334,154.807067 C133.370006,156.60937 131.748993,157.557118 130.599249,157.650341 C129.449504,157.743563 127.973502,156.889036 126.1712,155.086733 L88.9134394,117.828973 C88.4991169,117.414651 88.2142745,117.160882 88.0589036,117.067659 C87.9035327,116.974437 87.7533431,117.000332 87.6083302,117.145344 C84.9152343,122.324375 82.5432738,126.685053 80.4923777,130.22751 L81.5178206,131.252953 C81.9114269,130.610754 82.2480589,130.087679 82.5277265,129.683715 C82.8073942,129.279751 83.0922366,128.932761 83.3822623,128.642735 C84.6459458,127.379051 85.9199682,126.975093 87.2043678,127.430848 C87.9501482,127.720874 89.0066546,128.549506 90.3739187,129.91677 L120.733245,160.276096 C122.452683,161.995534 123.265778,163.492252 123.172556,164.766293 C123.079333,166.040335 122.162659,167.733852 120.422504,169.846897 Z M118.340544,123.919483 C118.568422,129.388539 120.743582,134.184249 124.86609,138.306758 C129.278624,142.719292 134.322925,145.008389 139.999142,145.174118 C145.67536,145.339847 150.502144,143.433992 154.479639,139.456497 C158.622864,135.313272 160.476929,130.413983 160.041891,124.758481 C159.606852,119.10298 157.328113,114.214048 153.205604,110.09154 C148.730922,105.616857 143.800559,103.37955 138.414367,103.37955 C133.028175,103.37955 128.284254,105.430415 124.182462,109.532207 C120.059953,113.654716 118.112667,118.450426 118.340544,123.919483 Z" id="nav-stripe-2" fillOpacity="0.8" fill="#4BC4B8" ref={(r) => this.stripeTwo = r} transform="translate(-300, 300)"></path> 
            <polygon id="nav-stripe-1" fillOpacity="0.8" fill="#A3D3A1" points="0 100 100 0 200 0 0 200" ref={(r) => this.stripeOne = r} transform="translate(-200, 200)"></polygon> 
            <path d="M0 500 L0 400 L400 0 L500 0 L475 25 L400 25 L400 100 L375 125 L300 125 L300 200 L200 300 L125 300 L125 375 Z"
                  id="nav-stripe-4"
                  fillOpacity="0.8"
                    fill="#E82549"
                    ref={(r) => this.stripeFour = r} transform="translate(-500, 500)"> 
              </path>
              {times(nSideStripes)((i) => {
                 const yStart = i * 500 / nSideStripes
                 const yEnd = (i + 1) * 500 / nSideStripes
                 const xStart = 500 - yStart
                 const xEnd = 500 - yEnd
                 return (
                   <polygon id={`nav-side-stripe-${i}`}
                            fill={Object.values(colors)[i]}
                            points={`${xStart},${yStart} 1000,${yStart} 1000,${yEnd} ${xEnd},${yEnd}`}/> 
                 )
               })} 
          
        <g id="nav-weekday-container" transform="translate(404, 27)">
          {stringToSymbols(weekday).map((symbol, index) => {
             return <ElementSVG key={symbol + '-' + index}
                                symbol={symbol}
                                transform="translate(300, -300)"
                                animationDelay={(animationDelay + index * 400) + 'ms'} 
                                animate={
                                  [{
                                    type: 'translate',
                                    from: '300 -300',
                                    to: `${index * 61} 0`
                                  }] }/>
           })}
        </g> 
            </g>
            <path d="M375 125 L300 125 L300 200 Z" id="nav-home-bg" fill="" ref={(r) => this.navHomeBg = r} fill="#FEEDB3" />
            <g id="nav-home" 
               transform="translate(304.000000, 127.000000)"
               onClick={onHomeClick}
               onMouseEnter={this.mouseOver}
               onMouseLeave={this.mouseOut}>
              <polygon fillOpacity="0" points="0,0 180,0, 180,70 0,70"/>
              <g ref={(r) => this.symbolH = r }>
                <ElementSVG symbol="H"
                            id="nav-symbol-h"
                            animationDelay={animationDelay + 'ms'}
                            transform="translate(300, -300)"
                            animate={[{ from: '300 -300', to: '0 0'}]} />
              </g>
              <g transform="translate(61, 0)" ref={(r) => this.symbolO = r }>
                <ElementSVG symbol="O"
                            id="nav-symbol-o"
                            animationDelay={(animationDelay + 400) + 'ms'}
                            transform="translate(300, -300)"
                            animate={[{ from: '300 -300', to: '0 0'}]} />
              </g>
              <g transform="translate(122, 0)" ref={(r) => this.symbolMe = r }>
                <RawElement id="nav-symbol-me"
                            groupBlock="custom" 
                            atomicNumber="3"
                            symbol="Me"
                            name="Sequoia"
                            atomicMass="4.8x10²⁸"
                            ionizationEnergy="458"
                            electronicConfiguration="1s²"
                            oxidationStates="+1"
                            animationDelay={(animationDelay + 800) + 'ms'}
                            transform="translate(300, -300)"
                            animate={[{ from: '300 -300', to: '0 0'}]} />
              </g>

              <path className="nav-home-selectd-bg"
                    ref={(r) => this.navHomeSelectedBg = r}
                    d="M-61 -80 L118 34 L-61 153 L-61 102 L0 102 L0 69 L61 69 L61 0 L0 0 L0 -33 L-61 -33 Z"
                    stroke="#000000"
                    strokeDasharray="1000"
                    strokeDashoffset="1000"
                    fillOpacity="0"
                    fill={randomColor} />
              
            </g>
            <g id="nav-title-container" transform="translate(130, 302)">
              {stringToSymbols(title).map((symbol, index) => {
                 return <ElementSVG key={symbol + '-' + index}
                                    symbol={symbol}
                                    transform="translate(300, -600)"
                                    animationDelay={(animationDelay + index * 400) + 'ms'} 
                                    animate={
                                      [{
                                        type: 'translate',
                                        from: '300 -600',
                                        to: `${index * 61} 0`
                                      }] }/>
               })}
            </g>
          </g>
        </svg> 
              )
  }
}
