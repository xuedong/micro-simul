#include <SFML/Graphics.hpp>
#include <iostream>
#include <string>
#include <sstream>

std::string septSegments(int n)
{
	if (n == 0)
	    return "1110111";
	if (n == 1)
	    return "0010010";
	if (n == 2)
	    return "1011101";
	if (n == 3)
	    return "1011011";
	if (n == 4)
	    return "0111010";
	if (n == 5)
	    return "1101011";
	if (n == 6)
	    return "1101111";
	if (n == 7)
	    return "1010010";
	if (n == 8)
	    return "1111111";
	if (n == 9)
	    return "1111011";
	else
	    return "0000000";
}

std::string jourSemaine(int n)
{
	if (n == 0)
	    return "Lundi";
	if (n == 1)
	    return "Mardi";
	if (n == 2)
	    return "Mercredi";
	if (n == 3)
	    return "Jeudi";
	if (n == 4)
	    return "Vendredi";
	if (n == 5)
	    return "Samedi";
	if (n == 6)
	    return "Dimanche";
	else
	    return "";
}

void affiche(sf::RenderWindow &App, sf::Shape &VSegment, std::string entree)
{
	if (entree[0] == '1')
	{
	    sf::Shape Segment = VSegment;
            Segment.Rotate(90);
            App.Draw(Segment);
	}

	if (entree[1] == '1')
	{
	    sf::Shape Segment = VSegment;
            App.Draw(Segment);
	}

	if (entree[2] == '1')
	{
	    sf::Shape Segment = VSegment;
            Segment.Move(100, 0);
            App.Draw(Segment);
	}

	if (entree[3] == '1')
	{
	    sf::Shape Segment = VSegment;
            Segment.Rotate(90);
            Segment.Move(0, 100);
            App.Draw(Segment);
	}

	if (entree[4] == '1')
	{
	    sf::Shape Segment = VSegment;
            Segment.Move(0, 100);
            App.Draw(Segment);
	}

	if (entree[5] == '1')
	{
	    sf::Shape Segment = VSegment;
            Segment.Move(100, 100);
            App.Draw(Segment);
	}

	if (entree[6] == '1')
	{
	    sf::Shape Segment = VSegment;
            Segment.Rotate(90);
            Segment.Move(0, 200);
            App.Draw(Segment);
	}
}

std::string string_of_int(int n)
{
    std::ostringstream str;
    str << n;
    std::string s = str.str();
    return s;
}

int main()
{
    sf::RenderWindow App(sf::VideoMode(1000, 500), "Montre");

    int ok, jour, mois, an, jour_semaine, h, m, s;

    while (App.IsOpened())
    {
	std::cin >> ok >> jour >> mois >> an >> jour_semaine >> h >> m >> s;
	an += 2000;

        sf::Event Event;
        while (App.GetEvent(Event))
        {
            if (Event.Type == sf::Event::Closed)
                App.Close();
        }

	if (ok == 1)
	{
            App.Clear();
            App.Draw(sf::Shape::Rectangle(0, 0, 1000, 500, sf::Color::White));

            sf::Shape VSegment;
            VSegment.AddPoint(0, 0, sf::Color::Black, sf::Color::White);
            VSegment.AddPoint(10, 10, sf::Color::Black, sf::Color::White);
            VSegment.AddPoint(10, 90, sf::Color::Black, sf::Color::White);
            VSegment.AddPoint(0, 100, sf::Color::Black, sf::Color::White);
            VSegment.AddPoint(-10, 90, sf::Color::Black, sf::Color::White);
            VSegment.AddPoint(-10, 10, sf::Color::Black, sf::Color::White);

            VSegment.SetOutlineWidth(1);
            VSegment.EnableFill(true);
            VSegment.EnableOutline(true);

            VSegment.Move(20, 20);

	    affiche(App, VSegment, septSegments(h/10));

	    VSegment.Move(140, 0);

	    affiche(App, VSegment, septSegments(h%10));

	    sf::Shape Point;
	    Point.AddPoint(0, 0, sf::Color::Black);
	    Point.AddPoint(10, 10, sf::Color::Black);
	    Point.AddPoint(20, 0, sf::Color::Black);
	    Point.AddPoint(10, -10, sf::Color::Black);
	    Point.Move(290, 80);

	    App.Draw(Point);

	    Point.Move(0, 80);

	    App.Draw(Point);

	    VSegment.Move(180, 0);

	    affiche(App, VSegment, septSegments(m/10));

	    VSegment.Move(140, 0);

	    affiche(App, VSegment, septSegments(m%10));

	    Point.Move(320, -80);

	    App.Draw(Point);

	    Point.Move(0, 80);

	    App.Draw(Point);

	    VSegment.Move(180, 0);

	    affiche(App, VSegment, septSegments(s/10));

	    VSegment.Move(140, 0);

	    affiche(App, VSegment, septSegments(s%10));

	    std::string date = jourSemaine(jour_semaine) + " "
		             + string_of_int(jour) + "/"
			     + string_of_int(mois) + "/"
			     + string_of_int(an);

	    sf::Font MyFont;
	    if (!MyFont.LoadFromFile("arial.ttf"))
	    {
		std::cerr << "Impossible de charger la police." << std::endl;
	    }

	    sf::String Text(date, MyFont, 60);
	    Text.Move(40, 300);
	    Text.SetColor(sf::Color::Black);

	    App.Draw(Text);

            App.Display();
	}
    }

    return EXIT_SUCCESS;
}
