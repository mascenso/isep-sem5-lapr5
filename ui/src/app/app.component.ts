import {Component, OnDestroy, OnInit} from '@angular/core';
import {LoginService} from "./services/loginService.service";
import {Router} from "@angular/router";

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {
  title = 'angularFirstApp';
  loggedIn: boolean = false;

  constructor(private loginService: LoginService, private router: Router) {
  }

  ngOnInit(): void {
    this.loginService.login$.subscribe( val => {
      this.loggedIn = val
      this.router.navigate(['home']);
    });
  }

}
