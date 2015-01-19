class MaybeString : Object {
    string? value;

    public MaybeString(string? value) {
        this.value = value;
    }

    public string to_string() {
        if(value != null)
            return "Just \"" + value.to_string() + "\"";
        else
            return "Nothing";
    }
}

class LocatorNode {
    string tag;
    int offset;
    MaybeString id;
    MaybeString class;

    public LocatorNode(string tag, int offset, string? id, string? class) {
        this.tag = tag;
        this.offset = offset;
        this.id = new MaybeString(id);
        this.class = new MaybeString(class);
    }

    public string to_string() {
        return "LocatorNode {lnTag = \"" + tag + "\""
            + ", lnOffset = " + offset.to_string()
            + ", lnId = " + id.to_string()
            + ", lnClass = " + class.to_string()
            + "}";
    }
}

static int main() {
    stdout.printf(new LocatorNode("a", 1, "asdf", null).to_string());
    return 0;
}