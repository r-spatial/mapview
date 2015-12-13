#rTree [![Build Status](https://travis-ci.org/leaflet-extras/RTree.svg?branch=master)](https://travis-ci.org/leaflet-extras/RTree)

A non-recursive R-Tree library in pure JavaScript with no dependencies.  Fork of [Jon-Carlos Rivera's fantastic library](https://github.com/imbcmdth/RTree) which sadly seems not to be maintained. MIT Licensed. 


##So far:

- Bug fix when deleting points.
- Common.js module.
- Updated tests.
- Factory function for constructor.
- Method for dealing with GeoJSON.
- All methods now accept callbacks.
- Query by bbox instead of rectangle. 
- Submit to NPM.
- Update examples.
- add closure
- add GruntFile
- fix syntax (make it pass jslint)
- more modular
- that bug with deleting

##API

-  *RTree* ( _[ Number **max_node_width**, Function **callback** ]_ )

###Parameters: 

-  **max_node_width** : _optional_ : The maximum width of a node before a split is performed[<sup>1</sup>](#f1).

###Returns: 

-  An empty **rTree** object.

###Usage: 

-  Make a new rTree with a max node width of 10:
- `var myRTree = RTree(10);`


##rTree.insert

-  **rTree.insert** ( Rectangle[<sup>3</sup>](#f3) **bounds**, Object **element**)

###Parameters: 

-  **bounds** : **required** : A minimally bounding box for **element**.
- **element** : **required** : An object to add to the R-Tree.

###Returns: 

-  True.

###Usage: 

-  Insert a 10x10 object that starts at position 10x10:
- `myRTree.insert({x:10, y:10, w:10, h:10}, myObject);`


##rTree.remove

-  **rTree.remove** ( Rectangle[<sup>3</sup>](#f3) **area** _[, Object **element**)

###Parameters: 

-  **area** : **required** : An area to search within.
- **element** : _optional_ : An object to remove from the R-Tree. If no object is specified, *all* elements that touch *area* are deleted.

###Returns: 

-  An array of leafs deleted from the R-Tree.

###Usage: 

- Deletes all object that touch the 10x10 rectangle starting at position 10x10:
- `var myDelCount = myRTree.delete({x:10, y:10, w:10, h:10});`
- Delete only specific object if it touches the 10x10 rectangle starting at position 10x10:
- `var myDelCount = myRTree.delete({x:10, y:10, w:10, h:10}, specific_object);`

##rTree.geoJSON:

- **rTree.geoJSON** ( Object or Array **geoJSON**)

###Parameters

- **geoJSON** : **required** : Either an Object representing a GeoJSON feature collection or an Array representing a list of GeoJSON features.


###Usage:

```JavaScript
myRTree.geoJSON({
	"type":"FeatureCollection",
	"features":[
		{
			"type":"Feature",
			"geometry":{
				"type":"Point",
				"coordinates":[100,1]
			},
			"properties":{
				"prop0":"value0"
			}
		},
		{
			"type":"Feature",
			"geometry":{
				"type":"LineString",
				"coordinates":[
					[100,0],
					[101,1]
				]
			},
			"properties":{
				"prop0":"value0"
			}
		}
	]
});
```

##rTree.bbox:

-  **rTree.bbox** ( Bounds **area**)

###Parameters

-  **area** : **required** : Area to search, this can either be represented by a single parameter bounds array `[[x1,y1],[x2,y2]]`, two parameters representing the southwest and northeast corners `[x1,y1],[x2,y2]`, or 4 parameters of `[x1,y1,x2,y2]`.

###Returns:

- An array of matched features.

###Usage:
- Search a 10x10 area that starts at position 10x10 (these are all equivalent):
- `var myObjects1 = myRTree.bbox([[10,10],[20,20]]);`
- `var myObjects2 = myRTree.bbox([[10,10],[20,20]]);`
- `var myObjects3 = myRTree.bbox([10,10],[20,20]);`
- `var myObjects4 = myRTree.bbox([10,10],[20,20]);`
- `var myObjects5 = myRTree.bbox(10,10,20,20);`
- `var myObjects6 = myRTree.bbox(10,10,20,20);`

##rTree.search

-  **RTree.search** ( Rectangle[<sup>3</sup>](#f3) **area** [, Boolean **return node**, Array **return_array** ])

###Parameters: 

-  **area** : **required** : An area to search within.
-  **return node** : _optional_ : Whether to return the entire node, mainly internal option.
-  **return array** : _optional_ : An existing array to add the results to, defaults to [], mainly internal option.

###Returns: 

-  An array of objects that overlap or touch **area**.

###Usage: 

-  Search a 10x10 area that starts at position 10x10:
- `var myObjects = myRTree.search({x:10, y:10, w:10, h:10});`


###Notes

<sup><a name="f1">1</a></sup> Default max node width is currently 6.

<sup><a name="f3">3</a></sup> A _Rectangle_ is **any** object with public x, y, w, h properties. The object itself is not saved or used directly but copies are made of its x, y, w, h properties.
