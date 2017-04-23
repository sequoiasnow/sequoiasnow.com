import React from 'react'


/**
 * @atom LightBox
 * @description
 *   The lightbox simply dimms the entire screen and presents children.
 *   by default these children are centered, but if that is not prefered
 *   they can be removed. A lightbox may also be changed in severity by
 *   passing a boolean to opacity and is directly controlled. 
 */
const LightBox = ({ active = false, children }) => {
  return (
    <div className={cn('lightbox', { 'lightbox--active': active })}>
      <div className="lightbox__container">
        {children}
      </div>
    </div>
  )
}

LightBox.propTypes = {
  /**
   * Is the lightbox currently active?
   */
  active: React.PropTypes.bool
}

export default LightBox
