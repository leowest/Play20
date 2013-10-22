# The Play data validation library

## Overview

The Play validation API aims to provide a comprehensive toolkit to validate data from any format against user defined rules, and transform them to other types.

It unifies the [[Form Validation API|ScalaForms]] and the [[Json validation API|ScalaJsonCombinators]].

Being based on the same concepts as the Json validation API already available in previous versions, it should feel familiar to developers who worked with it. The validation API is rather than a totally new design, a simple generalization of these concepts.

## Design

The validation API is designed around a core defined in package `play.api.data.mapping`, and "extensions". Each extension provides primitives to validate and serialize data from / to a particular format ([[Json | ScalaValidationJson]], [[form encoded request body | ScalaValidationForm]], etc.). If you need to support new type of vaidation, refer to the [[extensions documentation | ScalaValidationExtensions]] for more informations.

To learn more about data validation, please consult [[Validation and transformation with Rule|ScalaValidationRule]], for data serialization read [[Serialization with Write | ScalaValidationWrite]]. If you just want to figure all this out by yourself, please see the [[Cookbook|ScalaValidationCookbook]].
