describe('Teste de criar uma passagem', () => {

  beforeEach(() => {
    cy.visit('http://localhost:4200/login');
    cy.get('#role').select('Administrador');
    cy.get('#signUp').click();
    cy.get('[id^="Campus"]').click();
    cy.get('[routerlink="create-bridge"]').click();
  });




  it('Deve ser possivel criar edificio e receber mensagem de sucesso', () => {

    //rota a interceptar para receber o codigo de sucesso
    cy.intercept('POST', 'http://localhost:4200/api/bridges').as('rotaBridge');

    cy.get('#mat-select-value-1').click();  // Click on the dropdown to open it
    cy.get('mat-option').contains('A5182').click();  // Assuming 'A9426' is an option in the dropdown

  })







})
