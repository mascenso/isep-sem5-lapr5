describe('Teste para lista de Elevadores', () => {
  before(() => {
    //criar edificio para ver na tabela
    cy.visit('http://localhost:4200/login');
    cy.get('#role').select('Administrador');
    cy.get('#signUp').click();
    cy.get('[id^="Campus"]').click();

    //crio um building 
    cy.get('[routerlink="create-building"]').click();
    const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
    cy.get('#mat-input-0').type('A' + numeroAleatorio.toString());
    cy.get('#mat-input-1').type('10');
    cy.get('#mat-input-2').type('50');
    cy.get('#mat-input-3').type('Edificio Z');
    cy.get('#mat-input-4').type('TESTE');
    cy.get('.mdc-button__label').click();

    //crio um floor num building da dropdown
    cy.get('[id^="Campus"]').click();
    cy.get('[routerlink="create-floor"]').click();
    cy.intercept('POST', 'http://localhost:4000/api/floors').as('rotaFloor');

    cy.get('#dropdownCreateFloor').click();
    cy.get('.mat-mdc-select-placeholder').click({force: true });

    cy.contains('Edificio Z').click();

    cy.get('#mat-input-5').type("10");
    cy.get('#mat-input-6').type('10');
    cy.get('#mat-input-7').type('1');
    cy.get('#mat-input-8').type('Mais um floor de teste');
    cy.get('#saveNewFloor').click();

    //crio elevadores
    cy.get('[id^="Campus"]').click();
    cy.get('[routerlink="create-elevator"]').click();
    cy.intercept('POST', 'http://localhost:4000/api/elevators');

    cy.get('#mat-input-9').type('ELEV' + numeroAleatorio.toString());

    cy.get('#dropdownCreateElevator').click();
    //cy.get('.mat-mdc-select-placeholder').click({force: true });

    cy.contains('Edificio Z').click();

    cy.get('#selectFloorsElevator').click();
    cy.contains('Piso 1').click();

      cy.get('#selectFloorsElevator').type('{esc}');

    cy.get('#saveNewElevator').click({ force: true });

    //ir para a pagina correta do teste
    cy.get('[id^="Campus"]').click();
    cy.get('[routerlink="list-elevators"]').click();

  });


  it('Deve exibir a tabela corretamente e com valores que vai buscar a bd', () => {

    //compara com o ultimo da tabela pois é esse que criamos
    cy.get('#dropdownCreateElevator').click();
    cy.get('.mat-mdc-select-placeholder').click({force: true });

    cy.contains('Edificio Z').click();

    cy.get('table.mat-elevation-z8').should('exist');
    cy.get('.mat-mdc-header-row > .cdk-column-buildingId').should('exist');
    cy.get('.mat-mdc-header-row > .cdk-column-code').should('exist');
    cy.get('.mat-mdc-header-row > .cdk-column-floorList').should('exist');
    cy.get('.elevator-row > .cdk-column-buildingId').should('not.be.empty');
    cy.get('.elevator-row > .cdk-column-code').should('not.be.empty');
    cy.get('.elevator-row > .cdk-column-floorList').should('not.be.empty');
  });


});
