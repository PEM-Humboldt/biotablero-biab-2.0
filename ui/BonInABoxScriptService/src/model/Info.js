/**
 * BON in a Box - Script service
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 1.0.0
 * Contact: jean-michel.lord@mcgill.ca
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 *
 */

import ApiClient from '../ApiClient';
import InfoInputsValue from './InfoInputsValue';
import InfoOutputsValue from './InfoOutputsValue';
import InfoReferencesInner from './InfoReferencesInner';

/**
 * The Info model module.
 * @module model/Info
 * @version 1.0.0
 */
class Info {
    /**
     * Constructs a new <code>Info</code>.
     * @alias module:model/Info
     */
    constructor() { 
        
        Info.initialize(this);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj) { 
    }

    /**
     * Constructs a <code>Info</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/Info} obj Optional instance to populate.
     * @return {module:model/Info} The populated <code>Info</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new Info();

            if (data.hasOwnProperty('script')) {
                obj['script'] = ApiClient.convertToType(data['script'], 'String');
            }
            if (data.hasOwnProperty('description')) {
                obj['description'] = ApiClient.convertToType(data['description'], 'String');
            }
            if (data.hasOwnProperty('external_link')) {
                obj['external_link'] = ApiClient.convertToType(data['external_link'], 'String');
            }
            if (data.hasOwnProperty('inputs')) {
                obj['inputs'] = ApiClient.convertToType(data['inputs'], {'String': InfoInputsValue});
            }
            if (data.hasOwnProperty('outputs')) {
                obj['outputs'] = ApiClient.convertToType(data['outputs'], {'String': InfoOutputsValue});
            }
            if (data.hasOwnProperty('references')) {
                obj['references'] = ApiClient.convertToType(data['references'], [InfoReferencesInner]);
            }
        }
        return obj;
    }


}

/**
 * @member {String} script
 */
Info.prototype['script'] = undefined;

/**
 * @member {String} description
 */
Info.prototype['description'] = undefined;

/**
 * @member {String} external_link
 */
Info.prototype['external_link'] = undefined;

/**
 * @member {Object.<String, module:model/InfoInputsValue>} inputs
 */
Info.prototype['inputs'] = undefined;

/**
 * @member {Object.<String, module:model/InfoOutputsValue>} outputs
 */
Info.prototype['outputs'] = undefined;

/**
 * @member {Array.<module:model/InfoReferencesInner>} references
 */
Info.prototype['references'] = undefined;






export default Info;

