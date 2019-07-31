#include "tabbit.h"
#include <string>
#include <cmath>


tab_bit::ref::ref(slowo * what, slowo index) {
	t = what;	// tablica bitow
	i = index;	// index bitu
}

bool tab_bit::ref::operator=(bool v) {
	// ustawiamy i-ty(wartosc i zapisywana w zmiennej klasy ref) bit na wartosc v
	if (v == 1)
		set(i);
	else
		unset(i);

	return get(i);
}

bool tab_bit::ref::operator=(ref r) {
	// ustawiamy bit z this o indeksie i na wartosc bitu z r o indeksie r.i
	if (r.get(r.i) == 1)
		set(i);
	else
		unset(i);

	return get(i);
}

bool tab_bit::ref::operator[](int i) {
	// zwracamy i-ty bit
	return get(i);
}

bool tab_bit::ref::set(slowo i) {
	// ustawiamy i-ty bit na 1
	return t[i / rozmiarSlowa] |= (slowo)1 << (i % rozmiarSlowa);
}

bool tab_bit::ref::unset(slowo i) {
	return t[i / rozmiarSlowa] &= ~((slowo)1 << (i % rozmiarSlowa));
}

bool tab_bit::ref::get(slowo i) {
	return (t[i / rozmiarSlowa] >> (i % rozmiarSlowa)) & 1;
}

tab_bit::ref::operator bool() {
	return operator[](i);
}

tab_bit operator!(const tab_bit & tb)
{
	tab_bit temp(tb.dl);
	// i-ty bit this.tab to bit przeciwny do i-tego bitu tb.tab
	for (int i = 0; i < (temp.dl - 1) / temp.rozmiarSlowa + 1; i++)
		temp.tab[i] = ~tb.tab[i];

	return temp;
}

std::istream & operator>>(std::istream & we, tab_bit & tb)
{
	std::string bits;
	we >> bits;

	// dlugosc stringa jest nowa dlugoscia tablicy bitow
	tb.dl = bits.length();

	for (int i = 0; i < tb.dl; i++) {
		if (!(bits[i] == '0' || bits[i] == '1'))
			throw std::invalid_argument("Invalid bit value");
		tb.pisz(i, (bool)(bits[i] - '0'));
	}

	return we;
}

std::ostream & operator<<(std::ostream & wy, const tab_bit & tb)
{
	// bity sa pisane w odwrotnej kolejnosci
	// tj, najmniej znaczacy bit jest pierwszy
	// a najbardziej znaczacy - ostatni
	for (int i = 0; i < tb.rozmiar(); i++)
		wy << tb[i];
	return wy;
}

tab_bit::tab_bit(int rozm)
{
	// tworzymy tablice zawierajaca odpowiednia ilosc bitow
	// zaokraglana do gory do najblizszej wielokrotnosci rozmiaru slowa
	// tj, jesli rozm = 42, to tab = slowo[1]
	// jesli rozm = 64, to tab = slowo[1]
	// jesli rozm = 65, to tab = slowo[2], etc.
	dl = rozm;
	tab = new slowo[(rozm - 1) / rozmiarSlowa + 1];

	// zerujemy wszystkie bity
	ref t(tab);
	for (int i = 0; i < rozm; i++)
		t.unset(i);
}

tab_bit::tab_bit(slowo tb)
{
	dl = rozmiarSlowa;
	// tworzymy jedno-elementowa tablice, ktorej jedynym elementem jest tb
	tab = new slowo[1];
	tab[0] = tb;
}

tab_bit::tab_bit(const tab_bit & tb)
{
	// tworzymy tablice mogaca pomiescic wszystkie bity z tb.tab
	dl = tb.dl;
	tab = new slowo[(dl - 1) / rozmiarSlowa + 1];

	// przepisujemy bity z odpowiednich pozycji
	for (int i = 0; i < dl; i++)
		pisz(i, tb[i]);
}

tab_bit::tab_bit(tab_bit && tb)
{
	dl = tb.dl;
	tab = tb.tab;
	tb.dl = 0;
	tb.tab = nullptr;
}

tab_bit::tab_bit(std::initializer_list<bool> il)
{
	dl = il.size();
	tab = new slowo[(dl - 1) / rozmiarSlowa + 1];
	int i = 0;
	for (bool b : il) {
		pisz(i, b);
		i++;
	}
}

tab_bit & tab_bit::operator=(const tab_bit & tb)
{
	delete[] tab;
	dl = tb.dl;
	tab = new slowo[(dl - 1) / rozmiarSlowa + 1];
	for (int i = 0; i < dl; i++)
		pisz(i, tb[i]);
	return *this;
}

tab_bit & tab_bit::operator=(tab_bit && tb)
{
	dl = tb.dl;
	delete[] tab;
	tab = tb.tab;
	tb.dl = 0;
	tb.tab = nullptr;
	return *this;
}

tab_bit::~tab_bit()
{
	// usuwamy tablice tab
	delete[] tab;
}

bool tab_bit::czytaj(int i) const
{
	ref temp(tab);
	// czytamy odpowiedni bit
	return temp.get(i);
}

bool tab_bit::pisz(int i, bool b)
{
	ref t(tab);
	// jesli podana wartosc bitu jest rowna 1 to ustawiamy go na 1
	// w przeciwnym wypadku 'odustawiamy'

	if (b == true)
		return t.set(i);
	// else -> b == false
	return t.unset(i);
}

bool tab_bit::operator[](int i) const
{
	// czytamy bit o podanym indeksie
	return czytaj(i);
}

tab_bit::ref tab_bit::operator[](int i)
{
	// tworzymy obiekt ref posiadajacy ta sama tablice co this
	// oraz podany indeks
	ref temp(tab, i);
	return temp;
}

tab_bit tab_bit::operator|(const tab_bit & tb)
{
	if (dl != tb.dl)
		throw std::out_of_range("bit arrays have different amount of bits");

	tab_bit temp(*this);
	temp |= tb;
	return temp;
}

tab_bit & tab_bit::operator|=(const tab_bit & tb)
{
	// jesli i-ty bit tab == 0 oraz i-ty bit tb.tab == 0, to nowy i-ty bit = 0
	// w przeciwnym wypadku nowy i-ty bit = 1
	if (dl != tb.dl)
		throw std::out_of_range("bit arrays have different amount of bits");

	for (int i = 0; i < (dl - 1) / rozmiarSlowa + 1; i++)
		tab[i] = tab[i] | tb.tab[i];
	return *this;
}

tab_bit tab_bit::operator&(const tab_bit & tb)
{
	if (dl != tb.dl)
		throw std::out_of_range("bit arrays have different amount of bits");

	tab_bit temp(*this);
	temp &= tb;

	return temp;
}

tab_bit & tab_bit::operator&=(const tab_bit & tb)
{
	// jesli i-ty bit tab == 1 oraz i-ty bit tb.tab == 1, to nowy i-ty bit = 1
	// w przeciwnym wypadku nowy i-ty bit = 0
	if (dl != tb.dl)
		throw std::out_of_range("bit arrays have different amount of bits");

	for (int i = 0; i < (dl - 1) / rozmiarSlowa + 1; i++)
		tab[i] = tab[i] & tb.tab[i];
	return *this;
}

tab_bit tab_bit::operator^(const tab_bit & tb)
{
	if (dl != tb.dl)
		throw std::out_of_range("bit arrays have different amount of bits");

	tab_bit temp(*this);
	temp ^= tb;

	return temp;
}

tab_bit & tab_bit::operator^=(const tab_bit & tb)
{
	if (dl != tb.dl)
		throw std::out_of_range("bit arrays have different amount of bits");

	// jesli i-ty bit tab jest taki sam jak i-ty bit tb.tab, to nowy i-ty bit = 0
	// w przeciwnym przypadku nowy i-ty bit = 1
	for (int i = 0; i < (dl - 1) / rozmiarSlowa + 1; i++)
		tab[i] = tab[i] ^ tb.tab[i];
	return *this;
}
