using Gtk;
using WebKit;

public class ValaBrowser : Window {

    private Regex protocol_regex;

    private Entry address_bar;
    private WebView web_view;

    public ValaBrowser (string protocol = "https")
    {
        set_default_size (800, 600);

        create_widgets ();
        connect_signals ();
        this.address_bar.grab_focus ();
    }

    public void start (string url = "http://example.net") {
        this.web_view.load_uri(url);
        show_all();
    }

    private void create_widgets () {
        this.address_bar = new Entry ();
        this.web_view = new WebView ();


        var scrolled_window = new ScrolledWindow (null, null);
        scrolled_window.set_policy (PolicyType.AUTOMATIC, PolicyType.AUTOMATIC);
        scrolled_window.add (this.web_view);

        var box = new VBox (0);
        box.pack_start (this.address_bar, false);
        box.pack_start (scrolled_window, true);
        this.add (box);
    }

    private void connect_signals () {
        this.destroy.connect (Gtk.main_quit);

        this.address_bar.activate.connect (on_activate);

        this.web_view.notify["title"].connect((s, p) => {
                this.title = "%s - OHS".printf(this.web_view.title);
        });

        this.web_view.load_changed.connect ((ev) => {
                if (ev == LoadEvent.COMMITTED)
                    this.address_bar.text = this.web_view.get_uri();
        });
    }

    private void on_activate () {
        var url = this.address_bar.text;
        this.web_view.load_uri(url);
    }

    public static int main (string[] args) {
        Gtk.init(ref args);

        Soup.Session s = webkit_get_default_session();

        var browser = new ValaBrowser();
        browser.start ();

        Gtk.main();

        return 0;
    }
}