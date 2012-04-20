#include <KAboutData>
#include <language/util/debuglanguageparserhelper.h>

int main(int argc, char* argv[])
{
    KAboutData aboutData( "php-parser", 0, ki18n( "js-parser" ),
                          "1", ki18n("KDevelop ECMAScript parser debugging utility"), KAboutData::License_GPL,
                          ki18n( "(c) 2012 Andrew Udvare" ), KLocalizedString(), "http://www.kdevelop.org" );

    return KDevelopUtils::initAndRunParser<JsParser>(aboutData, argc, argv);
}

// kate: indent-width 4
