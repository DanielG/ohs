class Test : Object {
    public string a { get; set; }
    public string b { get; set; }
    public int i { get; set; }
    public Json.Array o { get; set; }
}

static void main() {
    Test t = new Test();
    t.a = "hello";
    t.b = "world";
    t.i = 1234;

    Test o = new Test();
    o.a = "lalal";
    o.b = "ggggg";
    o.i = 765432;

    t.o = new Json.Array();
    t.o.add_object_element(Json.gobject_serialize(o).get_object());
    t.o.add_object_element(Json.gobject_serialize(o).get_object());
    t.o.add_object_element(Json.gobject_serialize(o).get_object());

    size_t len;
    stdout.printf(Json.gobject_to_data(t, out len));
}