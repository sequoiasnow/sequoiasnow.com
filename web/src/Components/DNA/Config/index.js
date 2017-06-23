const CONFIG_UPDATE = 'CONFIG_UPDATE'

export const configReducer = (state = defaultConfig, action) => {
  if ( action.type == CONFIG_UPDATE )
    return action.config
  return state
}
