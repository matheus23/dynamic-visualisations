* render a 'literally []' for picture literals. Where the picture is literally included in the box.
    You can then move pictures from the drawing area into the box. (does that make sense? Maybe not)
    POSTPONE
    I don't think this makes sense. You'd ideally be able to change the literal again. So just show the source damnit!

* combine multiple points to a circle/rectangle, etc.
    * First only allow combining 2 points, then allow combining more than that to paths/shapes, etc.
        * ~~How do we decide what can be selected? I want a general picking mechanism!~~ DONE. Implemented it using something similar to `Reactive (V2 Double) Focus`. Now I can represent my selection as `[Focus]`.
    * If the user can choose, show him a dropdown at mouse position similar to how it works in blender!

* Make it recursive! Allow lets in lets
    * let the user choose the scope on which he works.
    * make it possible to select which values should be 'inspected' and shown on the right

* Build more visualisation types!
    * visualize more than vectors and forms: lists, numbers, strings, reactives, everything!
    * Try to build PGA (Projective geometric algebra) type stuff! Distinguish vectors and points and rotations! aw yiss

* Build types!
    * Make suggestions based on types!
        Funny thing: I can already make suggestions based on types, because I always combine actual values. There is no static phase. There is only ever runtime. I always have values. huh. I can even do stuff like not suggest a dividing operator, when I'm combining 23 with 0, because I _know_ its zero. Huh.