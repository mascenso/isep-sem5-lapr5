import {Component, OnInit} from '@angular/core';
import {LoadingSpinnerService} from "../services/loading-spinner.service";
import {AuthService} from "../services/auth.service";

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {
  isExpanded: boolean = false;

  userRole: string = '';

  constructor(private authService: AuthService,
              public spinnerService: LoadingSpinnerService) {}
  ngOnInit(): void {
    this.userRole = this.authService.userRole();
  }

  public onLogout() {
    this.authService.logout();
  }

  protected readonly MENU_ENTRIES = MENU_ENTRIES;
}

interface MenuEntry {
  title: string;
  icon: string;
  authorizedRoles: string[];
  redirectTo: string;
}

const MENU_ENTRIES: MenuEntry[] = [
  {
    title: "Users Management",
    icon: "person_add",
    authorizedRoles: ['', 'ADMINISTRATOR'],
    redirectTo: 'users'
  },
  {
    title: "Campus management",
    icon: "domain_add",
    authorizedRoles: ['','ADMINISTRATOR', 'CAMPUS_MANAGER'],
    redirectTo: 'campus'
  },
  {
    title: "Fleet management",
    icon: "precision_manufacturing",
    authorizedRoles: ['','ADMINISTRATOR', 'FLEET_MANAGER'],
    redirectTo: 'fleet'
  },
  {
    title: "Tasks management",
    icon: "list_alt",
    authorizedRoles: ['','ADMINISTRATOR', 'TASK_MANAGER', 'USER'],
    redirectTo: 'tasks'
  },
  {
    title: "3D visualization",
    icon: "view_in_ar",
    authorizedRoles: ['','ADMINISTRATOR', 'CAMPUS_MANAGER', 'FLEET_MANAGER', 'TASK_MANAGER'],
    redirectTo: 'view'
  }
]
