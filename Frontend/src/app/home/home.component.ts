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
    this.userRole = localStorage.getItem('role') ?? '';

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
    title: "Campus management",
    icon: "domain_add",
    authorizedRoles: ['Administrador', 'Gestor de Campus'],
    redirectTo: 'campus'
  },
  {
    title: "Fleet management",
    icon: "precision_manufacturing",
    authorizedRoles: ['Administrador', 'Gestor de Frota'],
    redirectTo: 'fleet'
  },
  {
    title: "Tasks management",
    icon: "list_alt",
    authorizedRoles: ['Administrador', 'Gestor de Tarefas'],
    redirectTo: 'tasks'
  },
  {
    title: "3D visualization",
    icon: "view_in_ar",
    authorizedRoles: ['Administrador', 'Gestor de Campus', 'Gestor de Frota', 'Gestor de Tarefas'],
    redirectTo: 'view'
  }
]
