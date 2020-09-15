var form = document.getElementById("form");
var email = document.getElementById("email");
var acc_num = document.getElementById("acc_num");
var pesel = document.getElementById("pesel");
var date = document.getElementById("date");
var acc_num_error = document.querySelector(".acc_num_error");
var pesel_error = document.querySelector(".pesel_error");
var email_error = document.querySelector(".email_error");
var date_error = document.querySelector(".date_error");


form.addEventListener("submit", function (event) {
    // Each time the user tries to send the data, we check
    // if their input was valid
    if (!accNumValidator()){
        acc_num_error.innerHTML = "Invalid Account Number";
        acc_num_error.className = "error active";
        event.preventDefault();
    }
    else{
        acc_num_error.innerHTML = "";
    }
    if (!peselValidator()){
        pesel_error.innerHTML = "Invalid Pesel";
        pesel_error.className = "error active";
        event.preventDefault();
    }
    else{
        pesel_error.innerHTML = "";
    }
    if (!emailValidator()){
        email_error.innerHTML = "Invalid Email";
        email_error.className = "error active";
        event.preventDefault();
    }
    else{
        email_error.innerHTML = "";
    }
    if (!dateValidator()){
        date_error.innerHTML = "Invalid Date";
        date_error.className = "error active";
        event.preventDefault();
    }
    else{
        date_error.innerHTML = "";
    }
}, false);

function accNumValidator(){
    return /^[0-9]{26}/.test(acc_num.value);
}

function peselValidator(){
    return /^[0-9]{10}/.test(pesel.value);
}

function emailValidator(){
    var re = /^(([^<>()\[\]\.,;:\s@\"]+(\.[^<>()\[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/i;
    return re.test(email.value);
}

function dateValidator(){
    // input type="date" does not allow non-numeric input anyway
    // if, say, user did write day and month but forgot to write year
    // the value of date will be ""
    return date.value !== "";
}